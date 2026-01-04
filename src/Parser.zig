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

pub const TopLevelStmt = union(enum) {
    fn_decl: FnDecl,
    extern_fn_decl: ExternFnDecl,
    // struct_decl
    // enum_decl
    // use_stmt
    // etc...

    pub const FnDecl = struct {
        name: Expr,
        body: []Stmt,
        ret_type: Type,
    };

    pub const ExternFnDecl = struct {
        name: Expr,
        arg_type: Type,
        ret_type: Type,
    };
};

// TODO: partial nodes
pub const Stmt = union(enum) {
    if_stmt: IfStmt,
    //extern_fn_decl: ExternFnDecl,
    //fn_decl: FnDecl,
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

    // TODO remove
    pub const ExternFnDecl = struct {
        name: Expr,
        arg_type: Type,
        ret_type: Type,

        //symbol: ?*Symbol,
    };

    // TODO remove
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
    };

    pub const Literal = struct {
        value: []const u8,
        span: Span,
        type: LiteralType,
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
            .BANG => Precedence.unary,
            .LEFT_PAREN, .LEFT_BRACKET => Precedence.call_access,

            .IDENTIFIER,
            .INTEGER, .FLOAT, .STRING, .CHARACTER,
            .TRUE, .FALSE, .NULL,
            => Precedence.primary,
            else => Precedence.none,  
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
    //statements: std.array_list.Aligned(Stmt, null),

    pub fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .alloc = allocator,
            .diagnostics = initDiagnostics(),
            //.statements = initStatements(),
        };
    }

    pub fn parseTokens(self: *Parser) ![]TopLevelStmt {
        var items: std.array_list.Aligned(TopLevelStmt, null) = .empty;

        while (!isAtEnd(self)) {
            if (try parseTopLevelStmt(self)) |stmt| {
                try items.append(self.alloc, stmt);
            }
        }
        
        return try items.toOwnedSlice(self.alloc);
    }

    pub fn getDiagnostics(self: *Parser) ![]Diagnostic {
        return try self.diagnostics.toOwnedSlice(self.alloc);
    }

    fn parseTopLevelStmt(self: *Parser) !?TopLevelStmt {
        return switch (getCurrentTokenType(self)) {
            .EXTERN => try externFnDeclaration(self) orelse return null,
            .FN => try fnDeclaration(self) orelse return null,
            else => {
                try self.collectError("expected top-level declaration", currToken(self));
                return null;
            }
        };
    }

    fn parseStatement(self: *Parser) anyerror!?Stmt {
        return switch (getCurrentTokenType(self)) {
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

        // fatal error (not a block)
        if (!try self.expect(.LEFT_BRACE, "expected '{'")) {
            return null;
        }

        while (!isAtEnd(self) and getCurrentTokenType(self) != .RIGHT_BRACE) {
            if (try parseStatement(self)) |stmt| {
                try stmt_list.append(self.alloc, stmt);
            }
        }

        _ = try self.expect(.RIGHT_BRACE, "expected '}'");

        return try stmt_list.toOwnedSlice(self.alloc);
    }

    fn parseExprStmt(self: *Parser) !?Stmt {
        const expr = try parseExpression(self) orelse return null;

        _ = try self.expect(.SEMICOLON, "expected ';'");
        
        return Stmt{ .expr_stmt = .{ .expr = expr } };
    }

    // === PRATT PARSER ===

    // TODO remove this unnececssary intermediary function
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
                        self.consume();
                        lit_type = .int;
                    },
                    .CHARACTER => {
                        self.consume();
                        lit_type = .char;
                    },
                    .FLOAT => {
                        self.consume();
                        lit_type = .float;
                    },
                    .STRING => {
                        self.consume();
                        lit_type = .string;
                    },
                    .TRUE => {
                        self.consume();
                        lit_type = .boolean;
                    },
                    .FALSE => {
                        self.consume();
                        lit_type = .boolean;
                    },
                    .NULL => {
                        self.consume();
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
                self.consume();
                return expr;
            },
            // grouping
            .LEFT_PAREN => {
                self.consume();

                const inner = try parseExpression(self) orelse return null;
                if (!try self.expect(.RIGHT_PAREN, "expected ')'")) {
                    try self.synchronize();
                    return null;
                }

                const expr = try self.alloc.create(Expr);
                expr.* = inner;
                return Expr{ .grouping = expr };
            },
            // unary
            .MINUS, .BANG => {
                const op: UnaryOp = if (curr.token_type == .MINUS) .NEG else .NOT;
                if (curr.token_type == .MINUS) {
                    self.consume();
                } else {
                    self.consume();
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
                        self.consume();
                        bin_op = .ADD;
                    },
                    .MINUS => {
                        self.consume();
                        bin_op = .SUB;
                    },
                    .STAR => {
                        self.consume();
                        bin_op = .MUL;
                    },
                    .SLASH => {
                        self.consume();
                        bin_op = .DIV;
                    },
                    .MODULUS => {
                        self.consume();
                        bin_op = .MODULUS;
                    },
                    .GREATER => {
                        self.consume();
                        bin_op = .GREATER;
                    },
                    .LESS => {
                        self.consume();
                        bin_op = .LESS;
                    },
                    .GREATER_EQUAL => {
                        self.consume();
                        bin_op = .GREATER_EQUAL;
                    },
                    .LESS_EQUAL => {
                        self.consume();
                        bin_op = .LESS_EQUAL;
                    },
                    .AND => {
                        self.consume();
                        bin_op = .AND;
                    },
                    .OR => {
                        self.consume();
                        bin_op = .OR;
                    },
                    .EQUAL_EQUAL => {
                        self.consume();
                        bin_op = .EQUAL_EQUAL;
                    },
                    .BANG_EQUAL => {
                        self.consume();
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
                self.consume();
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

                self.consume();

                if (currToken(self).token_type != .RIGHT_PAREN) {
                    while (true) {
                        try args.append(self.alloc, try self.parseExpression() orelse return null);                        
                        if (!(try match(self, .COMMA) orelse return null)) break;
                    }
                }

                _ = try self.expect(.RIGHT_PAREN, "expected ')' after function arguments");

                const expr = try self.alloc.create(Expr.CallExpr);
                expr.* = .{
                     .func_name = left,
                     .args = try args.toOwnedSlice(self.alloc),
                };
                return Expr{ .func_call = expr };
            },
            
            // TODO array indexing & member access?

            .PLUS_EQUAL => {
                self.consume();

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
                self.consume();
                
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
                self.consume();
                
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
                self.consume();
                
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
       if (!try self.expect(.WHILE, "expected 'while'")) {
           return null;
       }

       _ = try self.expect(.LEFT_PAREN, "expected '(' after 'while'");
       
       const cond = try parseExpression(self) orelse {
           try self.collectError("expected condition", currToken(self));
           try self.synchronize();
           return null;
       };

       _ = try self.expect(.RIGHT_PAREN, "expected ')'");

       const body = try parseBlock(self) orelse {
           try self.collectError("expected block, denoted by '{}'", currToken(self));
           try self.synchronize();
           return null;
       };

       return Stmt{ .while_loop = .{
           .cond = cond,
           .body = body,
       }};
    }

    fn fnDeclaration(self: *Parser) !?TopLevelStmt {
        if (!try self.expect(.FN, "expected 'fn'")) {
            return null;
        }

        const func_name = Expr{ .literal = .{
            .type = .string,
            .span = currToken(self).span.?,
            .value = currToken(self).lexeme,
        }};

        if (!try self.expect(.IDENTIFIER, "expected function name")) {
            return null;
        }

        if (!try self.expect(.LEFT_PAREN, "expected '(' after function name")) {
            try self.synchronize();
            return null;
        }
    
        // no params yet

        _ = try self.expect(.RIGHT_PAREN, "expected ')' after parameters");
        _ = try self.expect(.RIGHT_ARROW, "expected '=>' to signify type information");

        const ret_type = try parseType(self) orelse {
            try self.collectError("expected return type marker", currToken(self));
            try self.synchronize();
            return null;
        };

        const func_body = try parseBlock(self) orelse {
            try self.collectError("expected function body", currToken(self));
            try self.synchronize();
            return null;
        };

        // TODO: loop over stmts to see if there's a return stmt,
        // if not return error except if ret type is void, or some other way to check for a
        // return statement...
        
        return TopLevelStmt{ .fn_decl = .{
            .body = func_body,
            .name = func_name,
            .ret_type = ret_type,
        }};
    }

    fn ifStmt(self: *Parser) !?Stmt {
        if (!try self.expect(.IF, "expected 'if'")) {
            return null;
        }

        _ = try self.expect(.LEFT_PAREN, "expected '(' after 'if'");
        
        const cond = try parseExpression(self) orelse {
            try self.collectError("expected condition", currToken(self));
            try self.synchronize();
            return null;
        };

        _ = try self.expect(.RIGHT_PAREN, "expected ')' after condition");

        const if_body = try parseBlock(self) orelse {
            try self.collectError("expected block after 'if' condition", currToken(self));
            try self.synchronize();
            return null;
        };

        return Stmt{ .if_stmt = .{
            .condition = cond,
            .if_body = if_body,
        }};
    }
    
    fn retStmt(self: *Parser) !?Stmt {
        if (!try self.expect(.RETURN, "expected 'return'")) {
            return null;
        }
        
        const ret_value = try parseExpression(self) orelse {
            try self.collectError("expected return value", currToken(self));
            try self.synchronize();
            return null;
        };

        _ = try self.expect(.SEMICOLON, "expected ';'");

        return Stmt{ .ret_stmt = .{
            .value = ret_value,
        }};
    }

    // TODO: variable declaration without the value, like 'let x: i32;'
    fn variableDecl(self: *Parser) !?Stmt {
        if (!try self.expect(.LET, "expected 'let'")) {
            return null;
        }

        var mutable = false;
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            self.consume();
        }

        const var_name = try parseLiteral(self) orelse {
            try self.collectError("expected variable name", currToken(self));
            try self.synchronize();
            return null;
        };
        
        _ = try self.expect(.COLON, "expected ':'");

        // TODO: type inference!
        const var_type = try parseType(self) orelse {
            try self.collectError("expected type annotation", currToken(self));
            try self.synchronize();
            return null;
        };

        _ = try self.expect(.EQUAL, "expected '='");

        const value = try parseExpression(self) orelse {
            try self.collectError("expected variable initialization", currToken(self));
            try self.synchronize();
            return null;
        };

        const stmt = Stmt{ .var_decl = .{
            .mutable = mutable,
            .name = Expr{ .literal = var_name.literal },
            .value = value,
            .var_type = var_type,
        }};

        _ = try self.expect(.SEMICOLON, "expected ';'");

        return stmt;
    }

    fn externFnDeclaration(self: *Parser) !?TopLevelStmt {
        if (!try self.expect(.EXTERN, "expected 'extern'")) {
            return null;
        }
    
        if (!try self.expect(.FN, "expected 'fn'")) {
            return null;
        }

        const name = try parseLiteral(self) orelse {
            try self.collectError("expected function name", currToken(self));
            try self.synchronize();
            return null;
        };
        
        _ = try self.expect(.LEFT_PAREN, "expected '('");
        
        if (!try self.expect(.IDENTIFIER, "expected identifier")) {
            try self.synchronize();
            return null;
        }

        _ = try self.expect(.COLON, "expected ':'");

        const arg_type = try parseType(self) orelse {
            try self.collectError("expected argument type", currToken(self));
            try self.synchronize();
            return null;
        };

        _ = try self.expect(.RIGHT_PAREN, "expected ')'");

        if (!try self.expect(.RIGHT_ARROW, "expected '=>'")) {
            try self.synchronize();
            return null;
        }

        const ret_type = try parseType(self) orelse {
            try self.collectError("expected return type", currToken(self));
            try self.synchronize();
            return null;
        };

        _ = try self.expect(.SEMICOLON, "expected ';'");

        return TopLevelStmt{ .extern_fn_decl = .{
            .name = name,
            .arg_type = arg_type,
            .ret_type = ret_type,
        }};
    }

    fn parseType(self: *Parser) !?Type {
        const curr = currToken(self);

        switch (curr.token_type) {
            .C_INT => {
                self.consume();
                return .C_INT;
            },
            .UINT8 => {
                self.consume();
                return .UINT8;
            },
            .UINT16 => {
                self.consume();
                return .UINT16;
            },
            .UINT32 => {
                self.consume();
                return .UINT32;  
            },
            .UINT64 => {
                self.consume();
                return .UINT64;  
            },
            .INT8 => {
                self.consume();
                return .INT8;  
            },
            .INT16 => {
                self.consume();
                return .INT16;  
            },
            .INT32 => {
                self.consume();
                return .INT32;  
            },
            .INT64 => {
                self.consume();
                return .INT64;
            },
            .FLOAT32 => {
                self.consume();
                return .FLOAT32;  
            },
            .FLOAT64 => {
                self.consume();
                return .FLOAT64;  
            },
            .BOOL => {
                self.consume();
                return .BOOL;  
            },
            .VOID => {
                self.consume();
                return .VOID;
            },
            else => {
                // TODO see if taking previous token causes further issues, currently
                // it fixes some related to wrong error msgs
                try collectError(self, "expected a type", try previousToken(self));
                try synchronize(self);
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
                self.consume();
            },
            .INTEGER => {
                lit_type = .int;
                self.consume();
            },
            .FLOAT => {
                lit_type = .float;
                self.consume();
            },
            .TRUE => {
                lit_type = .boolean;
                self.consume();
            },
            .FALSE => {
                lit_type = .boolean;
                self.consume();  
            },
            .STRING => {
                lit_type = .string;
                self.consume();  
            },
            .CHARACTER => {
                lit_type = .int;
                self.consume();  
            },
            else => {
                try collectError(self, "expected a literal", try previousToken(self));
                try self.synchronize();
                return null;
            },
        }

        const lit = Expr{ .literal = .{
            .span = curr.span.?,
            .value = curr.lexeme,
            .type = lit_type,
        }};

        return lit;
    }

    // conditional advance
    fn expect(self: *Parser, token_type: TokenType, msg: []const u8) !bool {
        if (self.current >= self.tokens.len) {
            try self.collectError(msg, self.tokens[self.tokens.len - 1]);
            return false;
        }
        if (getCurrentTokenType(self) != token_type) {
            try self.collectError(msg, currToken(self));
            return false;
        }
        self.current += 1;
        return true;
    }

    // unconditional advance
    fn consume(self: *Parser) void {
        if (self.current < self.tokens.len) {
            self.current += 1;
        }
    }

    fn match(self: *Parser, token_type: TokenType) !?bool {
        if (currToken(self).token_type == token_type) {
            if (!try self.expect(token_type, "undefined message")) {
                try self.synchronize();
                return null;
            }
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
        return self.tokens[self.current].token_type == .EOF;
    }

    fn isAtStart(self: *Parser) bool {
        return self.current == 0;
    }

    fn indexFault(self: *Parser) bool {
        return self.current > self.tokens.len - 1;
    }

    // find a 'safe' token
    fn synchronize(self: *Parser) anyerror!void {
        while (!isAtEnd(self)) {
            if (getCurrentTokenType(self) == .SEMICOLON) {
                self.consume();
                return;
            }
            
            switch (getCurrentTokenType(self)) {
                .FN, .FOR, .WHILE, .IF, .RETURN, .LET, .EXTERN => return,
                // etc...
                else => {},
            }
            self.consume();
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
    NoTopLevelStmts,
    NotAFunction,
    EmptyFunction,
    NotAnExprStmt,
};

const Result = struct {
    ast: []TopLevelStmt,
    diagnostics: []Diagnostic,
};

fn parseSource(allocator: std.mem.Allocator, source: []const u8) !Result {
    var tokenizer = try Tokenizer.init(allocator, source);
    const tokens = try tokenizer.getTokens();

    var parser = Parser.init(tokens, allocator);
    const ast = try parser.parseTokens();
    const diagnostics = try parser.getDiagnostics();

    return Result{ .ast = ast, .diagnostics = diagnostics };
}

fn parseStmtInFunction(allocator: std.mem.Allocator, stmt: []const u8) !Result {
    const source = std.fmt.allocPrint(allocator,
        \\fn main() => void {{
        \\    {s}
        \\}}
    , .{stmt}) catch unreachable;

    return try parseSource(allocator, source);
}

fn parseExprInFunction(allocator: std.mem.Allocator, expr: []const u8) !Result {
    const source = std.fmt.allocPrint(allocator,
        \\fn main() => void {{
        \\    {s};
        \\}}
    , .{expr}) catch unreachable;

    return try parseSource(allocator, source);
}

fn getFirstExpr(result: anytype) !Expr {
    if (result.ast.len == 0) return error.NoTopLevelStmts;
    if (result.ast[0] != .fn_decl) return error.NotAFunction;

    const fn_body = result.ast[0].fn_decl.body;
    if (fn_body.len == 0) return error.EmptyFunction;
    if (fn_body[0] != .expr_stmt) return error.NotAnExprStmt;

    return fn_body[0].expr_stmt.expr;
}

fn getFirstStmt(result: anytype) !Stmt {
    if (result.ast.len == 0) return error.NoTopLevelStmts;
    if (result.ast[0] != .fn_decl) return error.NotAFunction;

    const fn_body = result.ast[0].fn_decl.body;
    if (fn_body.len == 0) return error.EmptyFunction;

    return fn_body[0];
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

    const result = try parseExprInFunction(allocator, "42");
    try expectNoErrors(result.diagnostics);

    const expr = try getFirstExpr(result);
    try testing.expect(expr == .literal);
    try testing.expectEqualStrings("42", expr.literal.value);
    try testing.expectEqual(LiteralType.int, expr.literal.type);
}

test "parse string literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseExprInFunction(allocator, "\"hello world\"");
    try expectNoErrors(result.diagnostics);

    const expr = try getFirstExpr(result);
    try testing.expect(expr == .literal);
    try testing.expectEqualStrings("hello world", expr.literal.value);
    try testing.expectEqual(LiteralType.string, expr.literal.type);
}

test "parse boolean literals" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    {
        const result = try parseExprInFunction(allocator, "true");
        try expectNoErrors(result.diagnostics);

        const expr = try getFirstExpr(result);
        try testing.expectEqual(LiteralType.boolean, expr.literal.type);
        try testing.expectEqualStrings("true", expr.literal.value);
    }

    {
        const result = try parseExprInFunction(allocator, "false");
        try expectNoErrors(result.diagnostics);
        
        const expr = try getFirstExpr(result);
        try testing.expectEqual(LiteralType.boolean, expr.literal.type);
        try testing.expectEqualStrings("false", expr.literal.value);
    }
}

test "parse binary expression: all operators" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const test_cases = [_]struct { source: []const u8, op: BinaryOp }{
        .{ .source = "1 + 2", .op = .ADD },
        .{ .source = "1 - 2", .op = .SUB },
        .{ .source = "1 * 2", .op = .MUL },
        .{ .source = "1 / 2", .op = .DIV },
        .{ .source = "1 % 2", .op = .MODULUS },
        .{ .source = "1 == 2", .op = .EQUAL_EQUAL },
        .{ .source = "1 != 2", .op = .NOT_EQUAL },
        .{ .source = "1 < 2", .op = .LESS },
        .{ .source = "1 <= 2", .op = .LESS_EQUAL },
        .{ .source = "1 > 2", .op = .GREATER },
        .{ .source = "1 >= 2", .op = .GREATER_EQUAL },
    };

    for (test_cases) |tc| {
        const result = try parseExprInFunction(allocator, tc.source);
        try expectNoErrors(result.diagnostics);

        const expr = try getFirstExpr(result);
        try testing.expect(expr == .binary);
        try testing.expectEqual(tc.op, expr.binary.op);
    }
}

test "parse operator precedence: multiplication before addition" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseExprInFunction(allocator, "1 + 2 * 3");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
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

    const result = try parseExprInFunction(allocator, "(1 + 2) * 3");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
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

    const result = try parseExprInFunction(allocator, "-42");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
    try testing.expect(expr == .unary);
    try testing.expectEqual(UnaryOp.NEG, expr.unary.op);
    
    try testing.expect(expr.unary.operand == .literal);
    try testing.expectEqualStrings("42", expr.unary.operand.literal.value);
}

test "parse unary expression: logical not" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseExprInFunction(allocator, "!true");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
    try testing.expect(expr == .unary);
    try testing.expectEqual(UnaryOp.NOT, expr.unary.op);
}

test "parse function call: no arguments" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseExprInFunction(allocator, "foo()");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
    try testing.expect(expr == .func_call);
    try testing.expectEqual(@as(usize, 0), expr.func_call.args.len);
    
    try testing.expect(expr.func_call.func_name == .literal);
    try testing.expectEqualStrings("foo", expr.func_call.func_name.literal.value);
}

test "parse function call: single argument" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseExprInFunction(allocator, "foo(42)");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
    try testing.expect(expr == .func_call);
    try testing.expectEqual(@as(usize, 1), expr.func_call.args.len);
    
    try testing.expect(expr.func_call.args[0] == .literal);
    try testing.expectEqualStrings("42", expr.func_call.args[0].literal.value);
}

test "parse function call: multiple arguments" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseExprInFunction(allocator, "foo(1, 2, 3)");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
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

    const result = try parseExprInFunction(allocator, "x = 42");
    try expectNoErrors(result.diagnostics);
    
    const expr = try getFirstExpr(result);
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
        .{ .source = "x += 1", .op = .ADD_EQUAL },
        .{ .source = "x -= 1", .op = .SUB_EQUAL },
        .{ .source = "x *= 2", .op = .MUL_EQUAL },
        .{ .source = "x /= 2", .op = .DIV_EQUAL },
    };
    
    for (test_cases) |tc| {
        const result = try parseExprInFunction(allocator, tc.source);
        try expectNoErrors(result.diagnostics);
        
        const expr = try getFirstExpr(result);
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

    const result = try parseStmtInFunction(allocator, "let x: i32 = 42;");
    try expectNoErrors(result.diagnostics);
    
    try testing.expectEqual(@as(usize, 1), result.ast.len);
        
    const first = try getFirstStmt(result);
    const decl = first.var_decl;
    try testing.expectEqual(false, decl.mutable);
    try testing.expectEqualStrings("x", decl.name.literal.value);
    try testing.expectEqualStrings("42", decl.value.literal.value);
}

test "parse variable declaration: mutable" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = try parseStmtInFunction(allocator, "let mut x: i32 = 42;");
    try expectNoErrors(result.diagnostics);
    
    const first = try getFirstStmt(result);
    const decl = first.var_decl;
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
    try testing.expectEqual(@as(usize, 0), decl.body.len);
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
    try testing.expectEqual(@as(usize, 1), decl.body.len);
    try testing.expect(decl.body[0] == .ret_stmt);
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
    
    const result = try parseStmtInFunction(allocator,
        \\if (true) {
        \\    x = 1;
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    const stmt = try getFirstStmt(result);
    const if_stmt = stmt.if_stmt;
    
    try testing.expect(if_stmt.condition == .literal);
    try testing.expectEqual(@as(usize, 1), if_stmt.if_body.len);
}

test "parse while loop" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseStmtInFunction(allocator,
        \\while (x < 10) {
        \\    x += 1;
        \\}
    );
    try expectNoErrors(result.diagnostics);

    const stmt = try getFirstStmt(result);
    const loop = stmt.while_loop;
    
    try testing.expect(loop.cond == .binary);
    try testing.expectEqual(@as(usize, 1), loop.body.len);
}

test "parse return statement" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseStmtInFunction(allocator, "return 42;");
    try expectNoErrors(result.diagnostics);

    const stmt = try getFirstStmt(result);
    const ret = stmt.ret_stmt;
    try testing.expectEqualStrings("42", ret.value.literal.value);
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
    try testing.expectEqual(@as(usize, 0), decl.body.len);
}

test "parse nested blocks" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseStmtInFunction(allocator, 
        \\if (true) {
        \\    if (false) {
        \\        x = 1;
        \\    }
        \\}
    );
    try expectNoErrors(result.diagnostics);
    
    const stmt = try getFirstStmt(result);
    const outer_if = stmt.if_stmt;
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
    
    const result = try parseStmtInFunction(allocator, "let x: i32 = 42");
    
    try testing.expect(result.diagnostics.len > 0);
    try expectError(result.diagnostics, "expected ';'");

    const decl = try getFirstStmt(result);
    try testing.expect(decl == .var_decl);
}

test "error: missing closing paren" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseStmtInFunction(allocator, "foo(42;");
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
    
    const result = try parseStmtInFunction(allocator, "let x: pekka = 42;");
    try testing.expect(result.diagnostics.len > 0);
    try expectError(result.diagnostics, "expected a type");
}

test "error recovery: continue after error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseStmtInFunction(allocator, 
        \\let x: i32 = 42
        \\let y: i32 = 10;
    );
    
    // should have error
    try testing.expect(result.diagnostics.len > 0);
}

test "recoverable error: multiple issues" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    // missing colon & semicolon
    const result = try parseStmtInFunction(allocator, "let x i32 = 42");
    
    // should report both errors
    try testing.expect(result.diagnostics.len >= 2);
    
    // but still parse further
    const stmt = try getFirstStmt(result);
    
    try testing.expect(stmt == .var_decl);
}

test "fatal error: missing identifier" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const result = try parseStmtInFunction(allocator, "let : i32 = 42;");
    
    try testing.expect(result.diagnostics.len > 0);
    
    // shouldn't create a declaration due to fatal error
    const res = getFirstStmt(result);
    try testing.expectError(error.EmptyFunction, res);
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
    try testing.expect(result.ast[1].fn_decl.body[0] == .var_decl);
    try testing.expect(result.ast[1].fn_decl.body[1] == .while_loop);
}

// TODO add:
// - complex expression tests
// - more edge cases
// - type (like i32) parsing tests
