const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Symbol = @import("Analyzer.zig").Symbol;
const SymbolTable = @import("Analyzer.zig").SymbolTable;
const Span = @import("main.zig").Span;

const Token = Tokenizer.Token;
const TokenType = Tokenizer.TokenType;
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

    pub fn parseTokens(self: *Parser) ![]Stmt {
        while (!isAtEnd(self)) {
            const stmt = try parseStatement(self);
            std.debug.print("{any}\n", .{stmt});
            if (stmt != null) try self.statements.append(self.alloc, stmt.?)
            else break;
        }
        
        const slice = try self.statements.toOwnedSlice(self.alloc);
        return slice;
    }

    pub fn getDiagnostics(self: *Parser) ![]Diagnostic {
        const slice = try self.diagnostics.toOwnedSlice(self.alloc);
        return slice;
    }

    fn parseStatement(self: *Parser) anyerror!?Stmt {
        const curr_token_type = getCurrentTokenType(self);

        switch (curr_token_type) {
            .EXTERN => {
                return try externFnDeclaration(self) orelse return null;
            },
            .FN => {
                return try fnDeclaration(self) orelse return null;  
            },
            .LET => {
                return try variableDecl(self) orelse return null;  
            },
            .IF => {
                return try ifStmt(self) orelse return null;
            },
            .IDENTIFIER => {
                return try parseExprStmt(self) orelse return null;
            },
            .RETURN => {
                std.debug.print("here1\n", .{});
                return try retStmt(self) orelse return null;
            },
            .WHILE => {
                return try whileLoop(self) orelse return null;
            },
            else => {},
        }
        return null;
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
                    .span = curr.span,
                    .value = curr.lexeme,
                    .type = lit_type,
                }};
                return expr;
            },
            .IDENTIFIER => {
                const expr = Expr{ .literal = .{
                    .span = curr.span,
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
            .span = currToken(self).span,
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

        std.debug.print("gggg\n", .{});

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
            .span = curr.span,
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
        print("Error at {d}:{d} : {s}, {any} | ", .{token.span.line, token.span.start_col, token.lexeme, token.token_type});
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
