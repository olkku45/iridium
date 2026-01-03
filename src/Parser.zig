const std = @import("std");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Symbol = @import("Analyzer.zig").Symbol;
const SymbolTable = @import("Analyzer.zig").SymbolTable;
const Span = @import("main.zig").Span;

const Token = @import("Tokenizer.zig").Token;
const TokenType = @import("Tokenizer.zig").TokenType;
const Category = TokenType.Category;

const print = std.debug.print;

const ParseError = error{
    WrongToken,
    WrongTokenType,
    MultiLineVarDecl,
    UnexpectedEOF,
    SomethingWentWrong,
    NotAStatement,
    IndexFault,
};

pub const BinaryOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
    MODULUS,
    EQUAL,
    EQUAL_EQUAL,
    NOT_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    ADD_EQUAL,
    SUB_EQUAL,
    MUL_EQUAL,
    DIV_EQUAL,
    AND,
    OR,
    none,
    // TODO add MOD_EQUAL
};

pub const UnaryOp = enum {
    NEG,
    NOT,  
};

pub const Type = enum {
    INT8,
    INT16,
    INT32,
    INT64,
    UINT8,
    UINT16,
    UINT32,
    UINT64,
    FLOAT32,
    FLOAT64,
    BOOL,
    VOID,
    C_INT,
    CUSTOM,
};

// TODO: partial nodes
pub const Stmt = union(enum) {
    if_stmt: IfStmt,
    extern_fn_decl: ExternFnDecl,
    fn_decl: FnDecl,
    var_decl: VariableDecl,
    expr_stmt: ExprStmt,
    ret_stmt: RetStmt,
    while_loop: WhileLoop,
    error_node: ErrorNode,

    pub const IfStmt = struct {
        condition: Expr,
        if_body: []Stmt,
        //else_body: []Stmt,
    };

    pub const ExternFnDecl = struct {
        name: Expr,
        arg_type: Type,
        ret_type: Type,

        //symbol: ?*Symbol,
    };

    pub const FnDecl = struct {
        name: Expr,
        fn_body: []Stmt,
        //params: , COMING SOON!!!
        ret_type: Type,

        //symbol: ?*Symbol,
    };

    pub const VariableDecl = struct {
        name: Expr,
        value: Expr,
        mutable: bool,
        var_type: Type,
        //symbol: ?*Symbol,
    };

    pub const ExprStmt = struct {
        expr: Expr,
    };

    pub const RetStmt = struct {
        value: Expr,
    };

    pub const WhileLoop = struct {
        cond: Expr,
        body: []Stmt,
    };

    pub const ErrorNode = struct {};
};

pub const Expr = union(enum) {
    literal: Literal,
    binary: *BinaryExpr,
    unary: *UnaryExpr,
    grouping: *Expr,
    func_call: *CallExpr,
    error_node: ErrorNode,

    pub const BinaryExpr = struct {
        left: Expr,
        op: BinaryOp,
        right: Expr,
    };

    pub const UnaryExpr = struct {
        op: UnaryOp,
        operand: Expr,
    };

    pub const CallExpr = struct {
        func_name: Expr,
        args: []Expr,

        //func_symbol: ?*Symbol,
        //ret_type: ?TypeAnnotation,
    };

    pub const Literal = struct {
        value: []const u8,
        span: Span,
        type: LiteralType,
        //annotation: ?TypeAnnotation,
    };

    pub const ErrorNode = struct {};
};

pub const LiteralType = enum {
    identifier,
    int,
    float,
    boolean,
    string,
    char,
    null,
    none,
};

pub const Diagnostic = struct {
    msg: ?[]const u8,
    severity: enum { err, warn, info },
    token: Token,
};

fn initDiagnostics() std.array_list.Aligned(Diagnostic, null) {
    const diagnostics: std.array_list.Aligned(Diagnostic, null) = .empty;
    return diagnostics;
}

fn initStatements() std.array_list.Aligned(Stmt, null) {
    const stmts: std.array_list.Aligned(Stmt, null) = .empty;
    return stmts;
}

pub const PrecedenceTable = struct {
    const Precedence = struct {
        const none = 0;
        const assignment = 10;
        const logical_or = 20;
        const logical_and = 30;
        const equality = 40;
        const relational = 50;
        const add_sub = 60;
        const mul_div_mod = 70;
        const unary = 80;
        const call_access = 90;
        const primary = 100;
    };

    fn getInfixPrecedence(token_type: TokenType) u8 {
        return switch (token_type) {
            .EQUAL, .PLUS_EQUAL, .MINUS_EQUAL, .STAR_EQUAL, .SLASH_EQUAL
             => Precedence.assignment,
            .OR => Precedence.logical_or,
            .AND => Precedence.logical_and,
            .EQUAL_EQUAL, .BANG_EQUAL => Precedence.equality,
            .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL => Precedence.relational,
            .PLUS, .MINUS => Precedence.add_sub,
            .STAR, .SLASH, .MODULUS => Precedence.mul_div_mod,
            .LEFT_PAREN, .LEFT_BRACKET => Precedence.call_access,
            else => {
                return Precedence.none;
            },  
        };
    }

    fn isRightAssociative(token_type: TokenType) bool {
        return switch (token_type) {
            .EQUAL, .PLUS_EQUAL, .MINUS_EQUAL, .STAR_EQUAL, .SLASH_EQUAL
            => true,
            else => false,  
        };
    }
};

pub const Parser = struct {
    tokens: []Token,
    current: usize,
    alloc: std.mem.Allocator,
    diagnostics: std.array_list.Aligned(Diagnostic, null),
    statements: std.array_list.Aligned(Stmt, null),

    pub fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .alloc = allocator,
            .diagnostics = initDiagnostics(),
            .statements = initStatements(),
        };
    }

    pub fn parseTokens(self: *Parser) !?[]Stmt {
        while (!isAtEnd(self)) {
            const stmt = try parseStatement(self);
            if (stmt) |s| {
                try self.statements.append(self.alloc, s);
            } else {
                if (isAtEnd(self)) break;

                try collectError(self, "unexpected token", currToken(self));
                try synchronize(self) orelse return null;
            }
        }
        
        return try self.statements.toOwnedSlice(self.alloc);
    }

    pub fn getDiagnostics(self: *Parser) ![]Diagnostic {
        const slice = try self.diagnostics.toOwnedSlice(self.alloc);
        return slice;
    }

    fn parseStatement(self: *Parser) anyerror!?Stmt {
        const curr_token_type = getCurrentTokenType(self);

        return switch (curr_token_type) {
            .EXTERN => try externFnDeclaration(self) orelse return null,
            .FN => try fnDeclaration(self) orelse return null,
            .LET => try variableDecl(self) orelse return null,
            .IF => try ifStmt(self) orelse return null,
            .RETURN => try retStmt(self) orelse return null,
            .WHILE => try whileLoop(self) orelse return null,
            // this does a couple things:
            // 1. handles all expression types
            // 2. returns error if not a valid expression
            else => try parseExprStmt(self) orelse return null,
        };
    }

    fn parseBlock(self: *Parser) !?[]Stmt {
        var stmt_list: std.array_list.Aligned(Stmt, null) = .empty;

        try advance(self, .LEFT_BRACE, "expected '{'") orelse return null;

        while (getCurrentTokenType(self) != .RIGHT_BRACE) {
            if (isAtEnd(self)) {
                try collectError(self, "unexpected end of file", currToken(self));

                const slice = try stmt_list.toOwnedSlice(self.alloc);
                return slice;
            }
            const stmt = try parseStatement(self);
            if (stmt != null) try stmt_list.append(self.alloc, stmt.?);
        }

        if (!isAtEnd(self)) {
            try advance(self, .RIGHT_BRACE, "expected '}'") orelse return null;
        }

        const slice = try stmt_list.toOwnedSlice(self.alloc);
        return slice;
    }

    fn parseExprStmt(self: *Parser) !?Stmt {
        const expr = try parseExpression(self) orelse return null;
        try advance(self, .SEMICOLON, "#59183 expected ';'") orelse return null;
        return Stmt{ .expr_stmt = .{ .expr = expr } };
    }

    // === PRATT PARSER ===
    
    fn parseExpression(self: *Parser) !?Expr {
        return try parseExpressionWithPrecedence(self, PrecedenceTable.Precedence.none) orelse return null;
    }

    fn parseExpressionWithPrecedence(
        self: *Parser,
        min_prec: u8
    ) !?Expr {
        var left = try parsePrefix(self) orelse return null;

        while (true) {
            const op_type = getCurrentTokenType(self);
            const precedence = PrecedenceTable.getInfixPrecedence(op_type);

            if (precedence <= min_prec) {
                break;
            }

            left = try parseInfix(self, left, precedence) orelse return null;
        }

        return left;
    }

    fn parsePrefix(self: *Parser) anyerror!?Expr {
        const curr = currToken(self);
        
        switch (curr.token_type) {
            // literals
            .INTEGER, .FLOAT, .STRING, .CHARACTER, .TRUE, .FALSE, .NULL => {
                var lit_type: LiteralType = undefined;
                
                switch (curr.token_type) {
                    .INTEGER => {
                        try advance(self, .INTEGER, "expected an int") orelse return null;
                        lit_type = .int;
                    },
                    .CHARACTER => {
                        try advance(self, .CHARACTER, "expected a char") orelse return null;
                        lit_type = .char;
                    },
                    .FLOAT => {
                        try advance(self, .FLOAT, "expected a float") orelse return null;
                        lit_type = .float;
                    },
                    .STRING => {
                        try advance(self, .STRING, "expected a string") orelse return null;
                        lit_type = .string;
                    },
                    .TRUE => {
                        try advance(self, .TRUE, "expected a boolean") orelse return null;
                        lit_type = .boolean;
                    },
                    .FALSE => {
                        try advance(self, .FALSE, "expected a boolean") orelse return null;
                        lit_type = .boolean;
                    },
                    .NULL => {
                        try advance(self, .NULL, "expected null") orelse return null;
                        lit_type = .null;
                    },
                    else => {},
                }
                
                const expr = Expr{ .literal = .{
                    .span = curr.span.?,
                    .value = curr.lexeme,
                    .type = lit_type,
                }};
                return expr;
            },
            .IDENTIFIER => {
                const expr = Expr{ .literal = .{
                    .span = curr.span.?,
                    .type = .identifier,
                    .value = curr.lexeme,
                }};
                try advance(self, .IDENTIFIER, "expected identifier") orelse return null;
                return expr;
            },
            // grouping
            .LEFT_PAREN => {
                try advance(self, .LEFT_PAREN, "expected '('") orelse return null;
                const inner = try parseExpression(self) orelse return null;
                try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;

                const expr = try self.alloc.create(Expr);
                expr.* = inner;
                return Expr{ .grouping = expr };
            },
            // unary
            .MINUS, .BANG => {
                const op: UnaryOp = if (curr.token_type == .MINUS) .NEG else .NOT;
                if (curr.token_type == .MINUS) {
                    try advance(self, .MINUS, "expected '-'") orelse return null;
                } else {
                    try advance(self, .BANG, "expected '!'") orelse return null;
                }
                const operand = try self.parseExpressionWithPrecedence(PrecedenceTable.Precedence.unary) orelse return null;
                               
                const expr = try self.alloc.create(Expr.UnaryExpr);
                expr.* = .{
                    .op = op,
                    .operand = operand,
                };
                return Expr{ .unary = expr };
            },

            else => {
                try collectError(self, "unknown error", curr);
                const err_node = Expr.ErrorNode{};
                const err = Expr{ .error_node = err_node };
                return err;
            },
        }
    }

    fn parseInfix(
        self: *Parser,
        left: Expr,
        prec: u8
    ) anyerror!?Expr {
        const op = currToken(self);

        return switch (op.token_type) {
            // binary
            .PLUS, .MINUS, .STAR, .SLASH, .MODULUS,
            .EQUAL_EQUAL, .BANG_EQUAL,
            .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL,
            .AND, .OR => {
                const next_prec: u8 =
                    if (PrecedenceTable.isRightAssociative(op.token_type)) prec
                    else prec + 1;

                var bin_op: BinaryOp = undefined;
                switch (op.token_type) {
                    .PLUS => {
                        try advance(self, .PLUS, "expected '+'") orelse return null;
                        bin_op = .ADD;
                    },
                    .MINUS => {
                        try advance(self, .MINUS, "expected '-'") orelse return null;
                        bin_op = .SUB;
                    },
                    .STAR => {
                        try advance(self, .STAR, "expected '*'") orelse return null;
                        bin_op = .MUL;
                    },
                    .SLASH => {
                        try advance(self, .SLASH, "expected '/'") orelse return null;
                        bin_op = .DIV;
                    },
                    .MODULUS => {
                        try advance(self, .MODULUS, "expected '%'") orelse return null;
                        bin_op = .MODULUS;
                    },
                    .GREATER => {
                        try advance(self, .GREATER, "expected '>'") orelse return null;
                        bin_op = .GREATER;
                    },
                    .LESS => {
                        try advance(self, .LESS, "expected '<'") orelse return null;
                        bin_op = .LESS;
                    },
                    .GREATER_EQUAL => {
                        try advance(self, .GREATER_EQUAL, "expected '>='") orelse return null;
                        bin_op = .GREATER_EQUAL;
                    },
                    .LESS_EQUAL => {
                        try advance(self, .LESS_EQUAL, "expected '<='") orelse return null;
                        bin_op = .LESS_EQUAL;
                    },
                    .AND => {
                        try advance(self, .AND, "expected 'and'") orelse return null;
                        bin_op = .AND;
                    },
                    .OR => {
                        try advance(self, .OR, "expected 'or'") orelse return null;
                        bin_op = .OR;
                    },
                    .EQUAL_EQUAL => {
                        try advance(self, .EQUAL_EQUAL, "expected '=='") orelse return null;
                        bin_op = .EQUAL_EQUAL;
                    },
                    .BANG_EQUAL => {
                        try advance(self, .BANG_EQUAL, "expected '!='") orelse return null;
                        bin_op = .NOT_EQUAL;
                    },
                    else => {},
                }
                
                const right = try self.parseExpressionWithPrecedence(next_prec) orelse return null;
                const expr = try self.alloc.create(Expr.BinaryExpr);
                expr.* = .{
                    .left = left,
                    .op = bin_op,
                    .right = right,
                };
                return Expr{ .binary = expr };
            },
            // assignment
            .EQUAL => {
                try advance(self, .EQUAL, "expected '='") orelse return null;
                const right = try self.parseExpressionWithPrecedence(prec) orelse return null;
                const bin_op: BinaryOp = .EQUAL;
    
                const expr = try self.alloc.create(Expr.BinaryExpr);
                expr.* = .{
                    .left = left,
                    .op = bin_op,
                    .right = right,
                };

                return Expr{ .binary = expr };
            },
            // function call
            .LEFT_PAREN => {
                var args: std.array_list.Aligned(Expr, null) = .empty;

                try advance(self, .LEFT_PAREN, "expected '('") orelse return null;

                if (currToken(self).token_type != .RIGHT_PAREN) {
                    while (true) {
                        try args.append(self.alloc, try self.parseExpression() orelse return null);                        
                        if (!(try match(self, .COMMA) orelse return null)) break;
                    }
                }

                try advance(self, .RIGHT_PAREN, "expected ')' after arguments") orelse return null;

                const expr = try self.alloc.create(Expr.CallExpr);
                expr.* = .{
                     .func_name = left,
                     .args = try args.toOwnedSlice(self.alloc),
                };
                return Expr{ .func_call = expr };
            },
            
            // TODO array indexing & member access?

            .PLUS_EQUAL => {
                try advance(self, .PLUS_EQUAL, "excepted '+='") orelse return null;
                const right = try self.parseExpressionWithPrecedence(prec) orelse return null;
                const bin_op: BinaryOp = .ADD_EQUAL;

                const expr = try self.alloc.create(Expr.BinaryExpr);
                expr.* = .{
                    .left = left,
                    .op = bin_op,
                    .right = right,
                };
                
                return Expr{ .binary = expr };
            },
            .MINUS_EQUAL => {
                try advance(self, .MINUS_EQUAL, "expected -=") orelse return null;
                const right = try self.parseExpressionWithPrecedence(prec) orelse return null;
                const bin_op: BinaryOp = .SUB_EQUAL;

                const expr = try self.alloc.create(Expr.BinaryExpr);
                expr.* = .{
                    .left = left,
                    .op = bin_op,
                    .right = right,
                };

                return Expr{ .binary = expr };
            },
            .STAR_EQUAL => {
                try advance(self, .STAR_EQUAL, "expected '*='") orelse return null;
                const right = try self.parseExpressionWithPrecedence(prec) orelse return null;
                const bin_op: BinaryOp = .MUL_EQUAL;

                const expr = try self.alloc.create(Expr.BinaryExpr);
                expr.* = .{
                    .left = left,
                    .op = bin_op,
                    .right = right,
                };

                return Expr{ .binary = expr };
            },
            .SLASH_EQUAL => {
                try advance(self, .SLASH_EQUAL, "expected '/='") orelse return null;
                const right = try self.parseExpressionWithPrecedence(prec) orelse return null;
                const bin_op: BinaryOp = .DIV_EQUAL;

                const expr = try self.alloc.create(Expr.BinaryExpr);
                expr.* = .{
                    .left = left,
                    .op = bin_op,
                    .right = right,
                };

                return Expr{ .binary = expr };
            },
            else => {
                const err = Expr.ErrorNode{};
                return Expr{ .error_node = err };
            },
        };
    }

    fn whileLoop(self: *Parser) !?Stmt {
       try advance(self, .WHILE, null) orelse return null;
       try advance(self, .LEFT_PAREN, "expected '('") orelse return null;
       
       const cond = try parseExpression(self) orelse return null;

       try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;

       const body = try parseBlock(self) orelse return null;

       return Stmt{ .while_loop = .{
           .cond = cond,
           .body = body,
       }};
    }

    fn fnDeclaration(self: *Parser) !?Stmt {
        try advance(self, .FN, null) orelse return null;

        const func_name = Expr{ .literal = .{
            .type = .string,
            .span = currToken(self).span.?,
            .value = currToken(self).lexeme,
        }};
        try advance(self, .IDENTIFIER, null) orelse return null;

        try advance(self, .LEFT_PAREN, "expected '(' after function name") orelse return null;

        // no params yet

        try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;
        try advance(self, .RIGHT_ARROW, "expected '=>'") orelse return null;

        const ret_type = try parseType(self) orelse return null;
        const func_body = try parseBlock(self) orelse return null;

        // TODO: loop over stmts to see if there's a return stmt,
        // if not return error except if ret type is void, or some other way to check for a
        // return statement...
        
        return Stmt{ .fn_decl = .{
            .fn_body = func_body,
            .name = func_name,
            .ret_type = ret_type,
        }};
    }

    fn ifStmt(self: *Parser) !?Stmt {
        try advance(self, .IF, "expected 'if'") orelse return null;
        try advance(self, .LEFT_PAREN, "expected '(' after if") orelse return null;
        
        const cond = try parseExpression(self) orelse return null;

        try advance(self, .RIGHT_PAREN, "expected ')' after condition") orelse return null;

        const if_body = try parseBlock(self) orelse return null;

        return Stmt{ .if_stmt = .{
            .condition = cond,
            .if_body = if_body,
        }};
    }
    
    fn retStmt(self: *Parser) !?Stmt {
        try advance(self, .RETURN, "expected 'return'") orelse return null;
        
        const ret_value = try parseExpression(self) orelse return null;

        // temporary comment, TODO uncomment this when codegen for naked return works,
        // not uncommenting will cause issues
        // try advance(self, .SEMICOLON, "expected ';'") orelse return null;

        return Stmt{ .ret_stmt = .{
            .value = ret_value,
        }};
    }

    // TODO: variable declaration without the value
    fn variableDecl(self: *Parser) !?Stmt {
        var mutable = false;

        try advance(self, .LET, null) orelse return null;
        
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            try advance(self, .MUT, null) orelse return null;
        }

        const var_name = try parseLiteral(self) orelse return null;
        
        try advance(self, .COLON, "expected ':'") orelse return null;
        
        const var_type = try parseType(self) orelse return null;

        try advance(self, .EQUAL, "expected '='") orelse return null;

        const value = try parseExpression(self) orelse return null;

        try advance(self, .SEMICOLON, "expected ';'") orelse return null;

        return Stmt{ .var_decl = .{
            .mutable = mutable,
            .name = Expr{ .literal = var_name.literal },
            .value = value,
            .var_type = var_type,
        }};
    }

    fn externFnDeclaration(self: *Parser) !?Stmt {
        try advance(self, .EXTERN, null) orelse return null;
        try advance(self, .FN, "expected 'fn'") orelse return null;

        const name = try parseLiteral(self) orelse return null;
        
        try advance(self, .LEFT_PAREN, "expected '('") orelse return null;
        try advance(self, .IDENTIFIER, null) orelse return null;
        try advance(self, .COLON, "expected ':'") orelse return null;

        const arg_type = try parseType(self) orelse return null;
        
        try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;
        try advance(self, .RIGHT_ARROW, "expected '=>'") orelse return null;

        const ret_type = try parseType(self) orelse return null;

        try advance(self, .SEMICOLON, "expected ';'") orelse return null;

        return Stmt{ .extern_fn_decl = .{
            .name = name,
            .arg_type = arg_type,
            .ret_type = ret_type,
        }};
    }

    fn parseBinOperator(self: *Parser) !?BinaryOp {
        const op = currToken(self);
        const m = "expected operator";

        switch (op.token_type) {
            .PLUS => {
                try advance(self, .PLUS, m) orelse return null;
                return .ADD;   
            },
            .MINUS => {
                try advance(self, .MINUS, m) orelse return null;
                return .SUB;
            },
            .STAR => {
                try advance(self, .STAR, m) orelse return null;
                return .MUL;
            },
            .SLASH => {
                try advance(self, .SLASH, m) orelse return null;
                return .DIV;  
            },
            .GREATER => {
                try advance(self, .GREATER, m) orelse return null;
                return .GREATER;
            },
            .LESS => {
                try advance(self, .LESS, m) orelse return null;
                return .LESS;
            },
            .EQUAL => {
                try advance(self, .EQUAL, m) orelse return null;
                return .EQUAL;
            },
            .BANG_EQUAL => {
                try advance(self, .BANG_EQUAL, m) orelse return null;
                return .NOT_EQUAL;
            },
            .EQUAL_EQUAL => {
                try advance(self, .EQUAL_EQUAL, m) orelse return null;
                return .EQUAL_EQUAL;
            },
            .LESS_EQUAL => {
                try advance(self, .LESS_EQUAL, m) orelse return null;
                return .LESS_EQUAL;
            },
            .GREATER_EQUAL => {
                try advance(self, .GREATER_EQUAL, m) orelse return null;
                return .GREATER_EQUAL;
            },
            .PLUS_EQUAL => {
                try advance(self, .PLUS_EQUAL, m) orelse return null;
                return .ADD_EQUAL;
            },
            .MINUS_EQUAL => {
                try advance(self, .MINUS_EQUAL, m) orelse return null;
                return .SUB_EQUAL;
            },
            .STAR_EQUAL => {
                try advance(self, .STAR_EQUAL, m) orelse return null;
                return .MUL_EQUAL;
            },
            .SLASH_EQUAL => {
                try advance(self, .SLASH_EQUAL, m) orelse return null;
                return .DIV_EQUAL;
            },
            else => {
                try collectError(self, "expected an operator", try previousToken(self));
                advanceSync(self) orelse return null;
            },
        }
        return null;
    }

    fn parseType(self: *Parser) !?Type {
        const curr = currToken(self);
        const m = "expected a type";

        switch (curr.token_type) {
            .C_INT => {
                try advance(self, .C_INT, m) orelse return null;
                return .C_INT;
            },
            .UINT8 => {
                try advance(self, .UINT8, m) orelse return null;
                return .UINT8;
            },
            .UINT16 => {
                try advance(self, .UINT16, m) orelse return null;
                return .UINT16;
            },
            .UINT32 => {
                try advance(self, .UINT32, m) orelse return null;
                return .UINT32;  
            },
            .UINT64 => {
                try advance(self, .UINT64, m) orelse return null;
                return .UINT64;  
            },
            .INT8 => {
                try advance(self, .INT8, m) orelse return null;
                return .INT8;  
            },
            .INT16 => {
                try advance(self, .INT16, m) orelse return null;
                return .INT16;  
            },
            .INT32 => {
                try advance(self, .INT32, m) orelse return null;
                return .INT32;  
            },
            .INT64 => {
                try advance(self, .INT64, m) orelse return null;
                return .INT64;
            },
            .FLOAT32 => {
                try advance(self, .FLOAT32, m) orelse return null;
                return .FLOAT32;  
            },
            .FLOAT64 => {
                try advance(self, .FLOAT64, m) orelse return null;
                return .FLOAT64;  
            },
            .BOOL => {
                try advance(self, .BOOL, m) orelse return null;
                return .BOOL;  
            },
            .VOID => {
                try advance(self, .VOID, m) orelse return null;
                return .VOID;
            },
            else => {
                // TODO see if taking previous token causes further issues, currently
                // it fixes some related to wrong error msgs
                try collectError(self, "expected a type", try previousToken(self));
                try synchronize(self) orelse return null;
            },
        }
        return null;
    }

    fn parseLiteral(self: *Parser) !?Expr {
        const curr = currToken(self);
        
        var lit_type: LiteralType = undefined;
        switch (curr.token_type) {
            .IDENTIFIER => {
                lit_type = .identifier;
                try advance(self, .IDENTIFIER, "expected an identifier") orelse return null;
            },
            .INTEGER => {
                lit_type = .int;
                try advance(self, .INTEGER, "expected an integer") orelse return null;
            },
            .FLOAT => {
                lit_type = .float;
                try advance(self, .FLOAT, "expected a float") orelse return null; 
            },
            .TRUE => {
                lit_type = .boolean;
                try advance(self, .TRUE, "expected a boolean") orelse return null;
            },
            .FALSE => {
                lit_type = .boolean;
                try advance(self, .FALSE, "expected a boolean") orelse return null;  
            },
            .STRING => {
                lit_type = .string;
                try advance(self, .STRING, "expected a string") orelse return null;  
            },
            .CHARACTER => {
                lit_type = .int;
                try advance(self, .CHARACTER, "expected a character") orelse return null;  
            },
            else => {
                try collectError(self, "expected a literal", try previousToken(self));
                advanceSync(self) orelse return null;
            },
        }

        const lit = Expr{ .literal = .{
            .span = curr.span.?,
            .value = curr.lexeme,
            .type = lit_type,
        }};

        return lit;
    }
    
    fn advance(self: *Parser, token_type: TokenType, msg: ?[]const u8) !?void {
        // this is just for debugging compiler
        if (indexFault(self)) {
            return ParseError.IndexFault;
        }
        if (isAtEnd(self)) {
            // TODO: move these error collects from end of file somewhere else, where
            // they don't collect an error from a correct end of file
            //try collectError(self, "unexpected end of file", currToken(self).span);
            return null;
        }
        if (getCurrentTokenType(self) != token_type) {
            try collectError(self, msg, try previousToken(self));
            try synchronize(self) orelse return null;
            return null;
        }
        self.current += 1;
    }

    fn advanceSync(self: *Parser) ?void {
        if (isAtEnd(self)) {
            // TODO: see the todo in advance()
            //try collectError(self, "unexpected end of file", currToken(self).span);
            return null;
        }
        self.current += 1;
    }

    fn match(self: *Parser, token_type: TokenType) !?bool {
        if (currToken(self).token_type == token_type) {
            try advance(self, token_type, null) orelse return null;
            return true;
        }
        return false;
    }

    fn currToken(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn nextToken(self: *Parser) !Token {
        if (isAtEnd(self)) {
            reportParseError(self.tokens[self.current]);
            return ParseError.WrongToken;
        }
        return self.tokens[self.current + 1];
    }

    fn previousToken(self: *Parser) !Token {
        if (isAtStart(self)) {
            reportParseError(self.tokens[self.current]);
            return ParseError.WrongToken;
        }
        return self.tokens[self.current - 1];
    }

    fn getCurrentTokenType(self: *Parser) TokenType {
        return self.tokens[self.current].token_type;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.current == self.tokens.len - 1;
    }

    fn isAtStart(self: *Parser) bool {
        return self.current == 0;
    }

    fn indexFault(self: *Parser) bool {
        return self.current > self.tokens.len - 1;
    }

    // find a 'safe' token
    fn synchronize(self: *Parser) anyerror!?void {
        while (!isAtEnd(self)) {
            if (getCurrentTokenType(self) == .SEMICOLON) {
                try advance(self, .SEMICOLON, null) orelse return null;
                return;
            }
            
            switch (getCurrentTokenType(self)) {
                .FN => return,
                .FOR => return,
                .WHILE => return,
                .IF => return,
                .RETURN => return,
                .LET => return,
                .EXTERN => return,
                // etc...
                else => {},
            }
            // can only happen at the end of a file
            if (advanceSync(self) == null) return null;
        }
    }

    fn reportParseError(token: Token) void {
        print("Error at {d}:{d} : {s}, {any} | ", .{token.span.?.line, token.span.?.start_col, token.lexeme, token.token_type});
    }

    fn collectError(self: *Parser, err_msg: ?[]const u8, token: Token) !void {
        const diag = Diagnostic{
            .msg = err_msg,
            .severity = .err,
            .token = token,
        };
        try self.diagnostics.append(self.alloc, diag);
    }
};

//  ====================================
//  TESTS
//  ====================================

const testing = std.testing;
const errors = error{
    UnexpectedParseErrors,
    ExpectedErrorNotFound,
};

fn parseSource(allocator: std.mem.Allocator, source: []const u8) !struct {
    ast: []Stmt,
    diagnostics: []Diagnostic,
} {
    var tokenizer = try Tokenizer.init(allocator, source);
    const tokens = try tokenizer.getTokens();

    var parser = Parser.init(tokens, allocator);
    const ast = try parser.parseTokens();
    const diagnostics = try parser.getDiagnostics();

    return .{ .ast = ast.?, .diagnostics = diagnostics };
}

fn expectNoErrors(diagnostics: []Diagnostic) !void {
    if (diagnostics.len > 0) {
        std.debug.print("\nUnexpected parse errors:\n", .{});
        for (diagnostics) |diag| {
            std.debug.print("  Line {any}: {s}\n", .{
                diag.token.span.?.line,
                diag.msg orelse "unknown error",
            });
        }
        return error.UnexpectedParseErrors;
    }
}

fn expectError(diagnostics: []Diagnostic, exp_msg: []const u8) !void {
    for (diagnostics) |diag| {
        if (diag.msg) |msg| {
            if (std.mem.indexOf(u8, msg, exp_msg) != null) {
                return;
            }
        }
    }
    std.debug.print("\nExpected error containing: {s}\n", .{exp_msg});
    std.debug.print("But got diagnostics:\n", .{});
    for (diagnostics) |diag| {
        std.debug.print("  {s}\n", .{diag.msg orelse "no message"});
    }
    return error.ExpectedErrorNotFound;
}

// ====================================
// EXPRESSION TESTS
// ====================================

test "parse integer literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseSource(allocator, "42;");
    try expectNoErrors(result.diagnostics);

    try testing.expectEqual(@as(usize, 1), result.ast.len);
    try testing.expect(result.ast[0] == .expr_stmt);

    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .literal);
    try testing.expectEqualStrings("42", expr.literal.value);
    try testing.expectEqual(LiteralType.int, expr.literal.type);
}

test "parse string literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseSource(allocator, "\"hello world\";");
    try expectNoErrors(result.diagnostics);

    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .literal);
    try testing.expectEqualStrings("hello world", expr.literal.value);
    try testing.expectEqual(LiteralType.string, expr.literal.type);
}

test "parse boolean literals" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    {
        const result = try parseSource(allocator, "true;");
        try expectNoErrors(result.diagnostics);
        const expr = result.ast[0].expr_stmt.expr;
        try testing.expectEqual(LiteralType.boolean, expr.literal.type);
        try testing.expectEqualStrings("true", expr.literal.value);
    }

    {
        const result = try parseSource(allocator, "false;");
        try expectNoErrors(result.diagnostics);
        const expr = result.ast[0].expr_stmt.expr;
        try testing.expectEqual(LiteralType.boolean, expr.literal.type);
        try testing.expectEqualStrings("false", expr.literal.value);
    }
}

test "parse binary expression: all operators" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const test_cases = [_]struct { source: []const u8, op: BinaryOp }{
        .{ .source = "1 + 2;", .op = .ADD },
        .{ .source = "1 - 2;", .op = .SUB },
        .{ .source = "1 * 2;", .op = .MUL },
        .{ .source = "1 / 2;", .op = .DIV },
        .{ .source = "1 % 2;", .op = .MODULUS },
        .{ .source = "1 == 2;", .op = .EQUAL_EQUAL },
        .{ .source = "1 != 2;", .op = .NOT_EQUAL },
        .{ .source = "1 < 2;", .op = .LESS },
        .{ .source = "1 <= 2;", .op = .LESS_EQUAL },
        .{ .source = "1 > 2;", .op = .GREATER },
        .{ .source = "1 >= 2;", .op = .GREATER_EQUAL },
    };

    for (test_cases) |tc| {
        const result = try parseSource(allocator, tc.source);
        try expectNoErrors(result.diagnostics);

        const expr = result.ast[0].expr_stmt.expr;
        try testing.expect(expr == .binary);
        try testing.expectEqual(tc.op, expr.binary.op);
    }
}

test "parse operator precedence: multiplication before addition" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "1 + 2 * 3;");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .binary);
    try testing.expectEqual(BinaryOp.ADD, expr.binary.op);

    // left = 1
    try testing.expect(expr.binary.left == .literal);
    try testing.expectEqualStrings("1", expr.binary.left.literal.value);

    // right = 2 * 3
    try testing.expect(expr.binary.right == .binary);
    try testing.expectEqual(BinaryOp.MUL, expr.binary.right.binary.op);
    try testing.expectEqualStrings("2", expr.binary.right.binary.left.literal.value);
    try testing.expectEqualStrings("3", expr.binary.right.binary.right.literal.value);
}

test "parse operator precedence: parentheses override" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "(1 + 2) * 3;");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .binary);
    try testing.expectEqual(BinaryOp.MUL, expr.binary.op);

    // left = grouping: (1 + 2)
    try testing.expect(expr.binary.left == .grouping);
    const grouped = expr.binary.left.grouping.*;
    try testing.expect(grouped == .binary);
    try testing.expectEqual(BinaryOp.ADD, grouped.binary.op);

    // right = 3
    try testing.expect(expr.binary.right == .literal);
    try testing.expectEqualStrings("3", expr.binary.right.literal.value);
}

test "parse unary expression: negation" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "-42;");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .unary);
    try testing.expectEqual(UnaryOp.NEG, expr.unary.op);
    
    try testing.expect(expr.unary.operand == .literal);
    try testing.expectEqualStrings("42", expr.unary.operand.literal.value);
}

test "parse unary expression: logical not" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "!true;");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .unary);
    try testing.expectEqual(UnaryOp.NOT, expr.unary.op);
}

test "parse function call: no arguments" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "foo();");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .func_call);
    try testing.expectEqual(@as(usize, 0), expr.func_call.args.len);
    
    try testing.expect(expr.func_call.func_name == .literal);
    try testing.expectEqualStrings("foo", expr.func_call.func_name.literal.value);
}

test "parse function call: single argument" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "foo(42);");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .func_call);
    try testing.expectEqual(@as(usize, 1), expr.func_call.args.len);
    
    try testing.expect(expr.func_call.args[0] == .literal);
    try testing.expectEqualStrings("42", expr.func_call.args[0].literal.value);
}

test "parse function call: multiple arguments" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "foo(1, 2, 3);");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .func_call);
    try testing.expectEqual(@as(usize, 3), expr.func_call.args.len);
    
    try testing.expectEqualStrings("1", expr.func_call.args[0].literal.value);
    try testing.expectEqualStrings("2", expr.func_call.args[1].literal.value);
    try testing.expectEqualStrings("3", expr.func_call.args[2].literal.value);
}

test "parse assignment" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "x = 42;");
    try expectNoErrors(result.diagnostics);
    
    const expr = result.ast[0].expr_stmt.expr;
    try testing.expect(expr == .binary);
    try testing.expectEqual(BinaryOp.EQUAL, expr.binary.op);
    
    try testing.expectEqualStrings("x", expr.binary.left.literal.value);
    try testing.expectEqualStrings("42", expr.binary.right.literal.value);
}

test "parse compound assignment operators" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const test_cases = [_]struct { source: []const u8, op: BinaryOp }{
        .{ .source = "x += 1;", .op = .ADD_EQUAL },
        .{ .source = "x -= 1;", .op = .SUB_EQUAL },
        .{ .source = "x *= 2;", .op = .MUL_EQUAL },
        .{ .source = "x /= 2;", .op = .DIV_EQUAL },
    };
    
    for (test_cases) |tc| {
        const result = try parseSource(allocator, tc.source);
        try expectNoErrors(result.diagnostics);
        
        const expr = result.ast[0].expr_stmt.expr;
        try testing.expect(expr == .binary);
        try testing.expectEqual(tc.op, expr.binary.op);
    }
}

// ====================================
// STATEMENT TESTS
// ====================================

test "parse variable declaration: immutable" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "let x: i32 = 42;");
    try expectNoErrors(result.diagnostics);
    
    try testing.expectEqual(@as(usize, 1), result.ast.len);
    try testing.expect(result.ast[0] == .var_decl);
    
    const decl = result.ast[0].var_decl;
    try testing.expectEqual(false, decl.mutable);
    try testing.expectEqualStrings("x", decl.name.literal.value);
    try testing.expectEqualStrings("42", decl.value.literal.value);
}

test "parse variable declaration: mutable" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "let mut x: i32 = 42;");
    try expectNoErrors(result.diagnostics);
    
    const decl = result.ast[0].var_decl;
    try testing.expectEqual(true, decl.mutable);
}

test "parse function declaration: void return" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\fn main() => void {
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    try testing.expect(result.ast[0] == .fn_decl);
    const decl = result.ast[0].fn_decl;
    try testing.expectEqualStrings("main", decl.name.literal.value);
    try testing.expectEqual(@as(usize, 0), decl.fn_body.len);
}

test "parse function declaration: with body" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\fn test() => i32 {
        \\    return 42;
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    const decl = result.ast[0].fn_decl;
    try testing.expectEqual(@as(usize, 1), decl.fn_body.len);
    try testing.expect(decl.fn_body[0] == .ret_stmt);
}

test "parse extern function declaration" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "extern fn putchar(c: c_int) => c_int;");
    try expectNoErrors(result.diagnostics);
    
    try testing.expect(result.ast[0] == .extern_fn_decl);
    const decl = result.ast[0].extern_fn_decl;
    try testing.expectEqualStrings("putchar", decl.name.literal.value);
}

test "parse if statement" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\if (true) {
        \\    x = 1;
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    try testing.expect(result.ast[0] == .if_stmt);
    const stmt = result.ast[0].if_stmt;
    
    try testing.expect(stmt.condition == .literal);
    try testing.expectEqual(@as(usize, 1), stmt.if_body.len);
}

test "parse while loop" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\while (x < 10) {
        \\    x = x + 1;
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    try testing.expect(result.ast[0] == .while_loop);
    const loop = result.ast[0].while_loop;
    
    try testing.expect(loop.cond == .binary);
    try testing.expectEqual(@as(usize, 1), loop.body.len);
}

test "parse return statement" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "return 42;");
    try expectNoErrors(result.diagnostics);
    
    try testing.expect(result.ast[0] == .ret_stmt);
    const stmt = result.ast[0].ret_stmt;
    try testing.expectEqualStrings("42", stmt.value.literal.value);
}

test "parse empty block" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\fn empty() => void {
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    const decl = result.ast[0].fn_decl;
    try testing.expectEqual(@as(usize, 0), decl.fn_body.len);
}

test "parse nested blocks" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\if (true) {
        \\    if (false) {
        \\        x = 1;
        \\    }
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    const outer_if = result.ast[0].if_stmt;
    try testing.expectEqual(@as(usize, 1), outer_if.if_body.len);
    
    try testing.expect(outer_if.if_body[0] == .if_stmt);
    const inner_if = outer_if.if_body[0].if_stmt;
    try testing.expectEqual(@as(usize, 1), inner_if.if_body.len);
}

// ====================================
// ERROR RECOVERY TESTS
// ====================================

test "error: missing semicolon" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "let x: i32 = 42");
    try testing.expect(result.diagnostics.len > 0);
    try expectError(result.diagnostics, "expected ';'");
}

test "error: missing closing paren" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "foo(42;");
    try testing.expect(result.diagnostics.len > 0);
    try expectError(result.diagnostics, "expected ')'");
}

test "error: missing closing brace" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\fn test() => void {
        \\    return;
    );
    try testing.expect(result.diagnostics.len > 0);
}

test "error: invalid type annotation" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, "let x: pekka = 42;");
    try testing.expect(result.diagnostics.len > 0);
    try expectError(result.diagnostics, "expected a type");
}

test "error recovery: continue after error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\let x: i32 = 42
        \\let y: i32 = 10;
    );
    
    // should have error
    try testing.expect(result.diagnostics.len > 0);
}

// ====================================
// PROGRAM TESTS
// ====================================

test "parse complete program" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseSource(allocator, 
        \\extern fn putchar(c: c_int) => c_int;
        \\
        \\fn main() => void {
        \\    let x: i32 = 0;
        \\    while (x < 10) {
        \\        putchar(48 + x);
        \\        x += 1;
        \\    }
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    try testing.expectEqual(@as(usize, 2), result.ast.len);
    try testing.expect(result.ast[0] == .extern_fn_decl);
    try testing.expect(result.ast[1] == .fn_decl);
    try testing.expect(result.ast[1].fn_decl.fn_body[0] == .var_decl);
    try testing.expect(result.ast[1].fn_decl.fn_body[1] == .while_loop);
}

// TODO add:
// - complex expression tests
// - more edge cases
// - type parsing tests
