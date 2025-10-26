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
};

pub const BinaryOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
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
};

pub const UnaryOp = enum {
    NEG,
    NOT,  
};

// TODO: move this to analyzer? would make more sense probably
pub const TypeAnnotation = union(enum) {
    named: NamedType,
    function: FunctionType,

    pub const NamedType = union(enum) {
        primitive: PrimitiveType,
        // user_defined: []const u8,  coming soon

        pub const PrimitiveType = enum {
            i8,
            i16,
            i32,
            i64,
            u8,
            u16,
            u32,
            u64,
            f32,
            f64,
            bool,
            void,
            c_int,
        };
    };

    pub const FunctionType = union(enum) {
        params: []TypeAnnotation,
        return_type: *TypeAnnotation,
    };
};

pub const Stmt = union(enum) {
    if_stmt: IfStmt,
    extern_fn_decl: ExternFnDecl,
    fn_decl: FnDecl,
    var_decl: VariableDecl,
    expr_stmt: ExprStmt,
    ret_stmt: RetStmt,
    error_node: ErrorNode,

    pub const IfStmt = struct {
        condition: Expr,
        if_body: []Stmt,
        //else_body: ?[]Stmt,
    };

    pub const ExternFnDecl = struct {
        name: Expr,
        arg_type: TypeAnnotation,
        ret_type: TypeAnnotation,

        symbol: ?*Symbol,
    };

    pub const FnDecl = struct {
        name: Expr,
        fn_body: []Stmt,
        //params: , COMING SOON!!!
        ret_type: TypeAnnotation,

        symbol: ?*Symbol,
    };

    pub const VariableDecl = struct {
        name: Expr,
        value: Expr,
        mutable: bool,
        var_type: TypeAnnotation,
        symbol: ?*Symbol,
    };

    pub const ExprStmt = struct {
        expr: Expr,
    };

    pub const RetStmt = struct {
        value: Expr,
    };

    pub const ErrorNode = struct {
        // TODO something here?
    };
};

pub const Expr = union(enum) {
    literal: Literal,
    binary: *BinaryExpr,
    unary: *UnaryExpr,
    grouping: *Expr,
    func_call: *CallExpr,
    var_upd: *VarUpdateExpr,

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

        func_symbol: ?*Symbol,
        ret_type: ?TypeAnnotation,
    };

    pub const VarUpdateExpr = struct {
        name: Expr,
        op: BinaryOp,
        value: Expr,

        var_symbol: ?*Symbol,
        var_type: ?TypeAnnotation,
    };

    pub const Literal = struct {
        value: []const u8,
        span: Span,
        //type: LiteralType,
        annotation: ?TypeAnnotation,
    };
};

pub const LiteralType = enum {
    variable,
    function,
    primitive,
};

pub const Diagnostic = struct {
    msg: ?[]const u8,
    span: Span,
    severity: enum { err, warn, info },  
};

fn initDiagnostics() std.array_list.Aligned(Diagnostic, null) {
    const diagnostics: std.array_list.Aligned(Diagnostic, null) = .empty;
    return diagnostics;
}

fn initStatements() std.array_list.Aligned(Stmt, null) {
    const stmts: std.array_list.Aligned(Stmt, null) = .empty;
    return stmts;
}

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
            try self.statements.append(self.alloc, stmt);
        }
        
        const slice = try self.statements.toOwnedSlice(self.alloc);
        return slice;
    }

    fn parseStatement(self: *Parser) anyerror!Stmt {
        const curr_token_type = getCurrentTokenType(self);
        var stmt: Stmt = undefined;

        switch (curr_token_type) {
            .EXTERN => {
                stmt = try externFnDeclaration(self);
            },
            .FN => {
                stmt = try fnDeclaration(self);  
            },
            .LET => {
                stmt = try variableDecl(self);  
            },
            .IF => {
                stmt = try ifStmt(self);
            },
            .IDENTIFIER => {
                stmt = try functionCall(self);    
            },
            .RETURN => {
                stmt = try retStmt(self);
            },
            else => {},
        }
        return stmt;
    }

    fn parseBlock(self: *Parser) ![]Stmt {
        var stmt_list: std.array_list.Aligned(Stmt, null) = .empty;

        try advance(self, .LEFT_BRACE, "expected '{'");

        while (getCurrentTokenType(self) != .RIGHT_BRACE) {
            if (isAtEnd(self)) {
                try collectError(self, "unexpected end of file", currToken(self).span);

                const err = Stmt{ .error_node = .{} };
                try self.statements.append(self.alloc, err);

                return stmt_list;
            }
            const stmt = try parseStatement(self);
            try stmt_list.append(self.alloc, stmt);
        }

        try advance(self, .RIGHT_BRACE, "expected '}'");

        const slice = stmt_list.toOwnedSlice(self.alloc);
        return slice;
    }

    fn fnDeclaration(self: *Parser) !Stmt {
        try advance(self, .FN, null);

        const func_name = try parseLiteral(self);

        try advance(self, .LEFT_PAREN, "expected '(' after function name");

        // no args for now

        try advance(self, .RIGHT_PAREN, "expected ')'");
        try advance(self, .RIGHT_ARROW, "expected '->'");

        const ret_type = try parseType(self);
        const func_body = try parseBlock(self);

        // TODO: loop over stmts to see if there's a return stmt,
        // if not return error except if ret type is void, or some other way to check for a
        // return statement...
        
        return Stmt{ .fn_decl = .{
            .fn_body = func_body,
            .name = func_name,
            .ret_type = TypeAnnotation{
                .named = .{
                    .primitive = ret_type,
                },
            },
            .symbol = null,
        }};
    }

    fn ifStmt(self: *Parser) !Stmt {
        try advance(self, .IF, null);
        try advance(self, .LEFT_PAREN, "expected '(' after if");

        // just a binary expression as condition for now
        // TODO: generalize for all expression types
        const cond = try binaryExpr(self);

        try advance(self, .RIGHT_PAREN, "expected ')' after condition");

        const if_body = try parseBlock(self);

        return Stmt{ .if_stmt = .{
            .condition = cond,
            .if_body = if_body,
        }};
    }

    fn binaryExpr(self: *Parser) !Stmt {
        const left = try parseLiteral(self);
        const op = parseBinOperator(self);
        const right = try parseLiteral(self);

        const binary = try self.alloc.create(Expr.BinaryExpr);
        binary.* = .{
            .left = left,
            .op = op,
            .right = right,
        };

        return Stmt { .expr_stmt = .{
            .expr = .{
                .binary = binary,
            }
        }};
    }

    fn retStmt(self: *Parser) !Stmt {
        try advance(self, .RETURN, null);
        
        const ret_value = try parseLiteral(self);

        try advance(self, .SEMICOLON, "expected ';' as line break");

        return Stmt{ .ret_stmt = .{
            .value = ret_value,
        }};
    }

    fn functionCall(self: *Parser) !Stmt {
        const ident = try parseLiteral(self);

        try advance(self, .LEFT_PAREN, null);

        var curr_type = currToken(self).token_type;

        // TODO: probably a simpler way to do this...
        var args: std.array_list.Aligned(Expr, null) = .empty;
        // just one function arg supported at the moment
        while (curr_type != .RIGHT_PAREN) {
            switch (curr_type) {
                .STRING => {
                    const str_arg = try parseLiteral(self);
                    try args.append(self.alloc, str_arg);
                },
                .IDENTIFIER => {
                    const ident_arg = try parseLiteral(self);
                    try args.append(self.alloc, ident_arg);  
                },
                .CHARACTER => {
                    const char_arg = try parseLiteral(self);
                    try args.append(self.alloc, char_arg);  
                },
                else => {},
            }
            curr_type = currToken(self).token_type;
        }

        const args_slice = try args.toOwnedSlice(self.alloc);

        try advance(self, .RIGHT_PAREN, "expected ')'");
        try advance(self, .SEMICOLON, "expected ';'");

        const call = try self.alloc.create(Expr.CallExpr);
        call.* = .{
            .func_name = Expr{ .literal = .{
                .value = ident.literal.value,
                .span = ident.literal.span,
                .type = .function,
                .annotation = null, // TODO should this be null?
            }},
            .args = args_slice,
            .func_symbol = null,
            .ret_type = null,  
        };

        return Stmt{ .expr_stmt = .{
            .expr = Expr {
                .func_call = call,
            }
        }};
    }

    // TODO: variable declaration without the value
    
    fn variableDecl(self: *Parser) !Stmt {
        var mutable = false;

        try advance(self, .LET, null);
        
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            try advance(self, .MUT, null);
        }

        const var_name = try parseLiteral(self);
        try advance(self, .COLON, "expected ':'");
        
        const var_type = try parseType(self);

        try advance(self, .EQUAL, "expected '='");

        const value = try parseLiteral(self);

        try advance(self, .SEMICOLON, "expected ';'");

        return Stmt{ .var_decl = .{
            .mutable = mutable,
            .name = var_name,
            .value = Expr{ .literal = value.literal },
            .var_type = TypeAnnotation{ .named = .{
                .primitive = var_type,
            }},
            .symbol = null,
        }};
    }

    fn externFnDeclaration(self: *Parser) !Stmt {
        try advance(self, .EXTERN, null);
        try advance(self, .FN, "expected 'fn'");

        const lit = try parseLiteral(self);
        
        try advance(self, .LEFT_PAREN, "expected ')'");

        try advance(self, .IDENTIFIER, null);

        try advance(self, .COLON, "expected ':'");

        const arg_type = try parseType(self);
        
        try advance(self, .RIGHT_PAREN, "expected ')'");
        try advance(self, .RIGHT_ARROW, "expected '->'"); // TODO change to '=>'

        const ret_type = try parseType(self);

        try advance(self, .SEMICOLON, "expected ';'");

        return Stmt{ .extern_fn_decl = .{
            .name = lit,
            .arg_type = TypeAnnotation{ .named = .{
                .primitive = arg_type,
            }},
            .ret_type = TypeAnnotation{ .named = .{
                .primitive = ret_type,
            }},
            .symbol = null,
        }};
    }

    fn parseBinOperator(self: *Parser) !?BinaryOp {
        const op = currToken(self);
        const m = "expected operator";

        switch (op.token_type) {
            .PLUS => {
                try advance(self, .PLUS, m);
                return .ADD;   
            },
            .MINUS => {
                try advance(self, .MINUS, m);
                return .SUB;
            },
            .STAR => {
                try advance(self, .STAR, m);
                return .MUL;
            },
            .SLASH => {
                try advance(self, .SLASH, m);
                return .DIV;  
            },
            .GREATER => {
                try advance(self, .GREATER, m);
                return .GREATER;
            },
            .LESS => {
                try advance(self, .LESS, m);
                return .LESS;
            },
            .EQUAL => {
                try advance(self, .EQUAL, m);
                return .EQUAL;
            },
            .BANG_EQUAL => {
                try advance(self, .BANG_EQUAL, m);
                return .NOT_EQUAL;
            },
            .EQUAL_EQUAL => {
                try advance(self, .EQUAL_EQUAL, m);
                return .EQUAL_EQUAL;
            },
            .LESS_EQUAL => {
                try advance(self, .LESS_EQUAL, m);
                return .LESS_EQUAL;
            },
            .GREATER_EQUAL => {
                try advance(self, .GREATER_EQUAL, m);
                return .GREATER_EQUAL;
            },
            .PLUS_EQUAL => {
                try advance(self, .PLUS_EQUAL, m);
                return .ADD_EQUAL;
            },
            .MINUS_EQUAL => {
                try advance(self, .MINUS_EQUAL, m);
                return .SUB_EQUAL;
            },
            .STAR_EQUAL => {
                try advance(self, .STAR_EQUAL, m);
                return .MUL_EQUAL;
            },
            .SLASH_EQUAL => {
                try advance(self, .SLASH_EQUAL, m);
                return .DIV_EQUAL;
            },
            else => {},
        }
        return null;
    }

    fn parseType(self: *Parser) !?TypeAnnotation.NamedType.PrimitiveType {
        const curr = currToken(self);
        const m = "expected type";

        switch (curr.token_type) {
            .C_INT => {
                try advance(self, .C_INT, m);
                return .c_int;
            },
            .UINT8 => {
                try advance(self, .UINT8, m);
                return .u8;
            },
            .UINT16 => {
                try advance(self, .UINT16, m);
                return .u16;
            },
            .UINT32 => {
                try advance(self, .UINT32, m);
                return .u32;  
            },
            .UINT64 => {
                try advance(self, .UINT64, m);
                return .u64;  
            },
            .INT8 => {
                try advance(self, .INT8, m);
                return .i8;  
            },
            .INT16 => {
                try advance(self, .INT16, m);
                return .i16;  
            },
            .INT32 => {
                try advance(self, .INT32, m);
                return .i32;  
            },
            .INT64 => {
                try advance(self, .INT64, m);
                return .i64;
            },
            .FLOAT32 => {
                try advance(self, .FLOAT32, m);
                return .f32;  
            },
            .FLOAT64 => {
                try advance(self, .FLOAT64, m);
                return .f64;  
            },
            .BOOL => {
                try advance(self, .BOOL, m);
                return .bool;  
            },
            .VOID => {
                try advance(self, .VOID, m);
                return .void;
            },
            else => {
                collectError(self, "expected a type", curr.span);
            },
        }
        return null;
    }

    fn parseLiteral(self: *Parser) !Expr {
        const curr = currToken(self);

        switch (curr.token_type) {
            .IDENTIFIER => try advance(self, .IDENTIFIER, "expected an identifier"),
            .INTEGER => try advance(self, .INTEGER, "expected integer"),
            .FLOAT => try advance(self, .FLOAT, "expected float"),
            else => {},
        }

        const lit = Expr{ .literal = .{
            .span = curr.span,
            .value = curr.lexeme,
            .annotation = null,
        }};

        return lit;
    }
    
    fn advance(self: *Parser, token_type: TokenType, msg: ?[]const u8) !void {
        if (isAtEnd(self)) {
            try collectError(self, "unexpected end of file", currToken(self).span);
            const err = Stmt{ .error_node = .{} };
            try self.statements.append(self.alloc, err);
            
            return;
        }
        if (getCurrentTokenType(self) != token_type) {
            try collectError(self, msg, currToken(self).span);
            appendErrorAndSync(self);
        }
        if (token_type.category() == .KEYWORD) {
            try checkKeyword(self, currToken(self));
        }
        self.current += 1;
    }

    fn advanceSync(self: *Parser) void {
        if (isAtEnd(self)) {
            try collectError(self, "unexpected end of file", currToken(self).span);
            return;
        }
        self.current += 1;
    }

    fn appendErrorAndSync(self: *Parser) !void {
        const err = Stmt{ .error_node = .{} };
        try self.statements.append(self.alloc, err);
        try synchronize(self);
    }

    fn checkKeyword(self: *Parser, token: Token) !void {
        switch (token.token_type) {
            .EXTERN => {
                const next = try nextToken(self);
                if (next.token_type != .FN) {
                    try collectError(self, "expected 'fn'", currToken(self).span);
                    appendErrorAndSync(self);
                }
            },
            // etc...
            else => {},
        }
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

    fn synchronize(self: *Parser) !void {
        while (!isAtEnd(self)) {
            advanceSync(self);

            if (getCurrentTokenType(self) == .SEMICOLON) {
                try advance(self, .SEMICOLON, null);
                return;
            }

            switch (getCurrentTokenType(self)) {
                .FN => return,
                .FOR => return,
                .WHILE => return,
                .IF => return,
                .RETURN => return,
                .LET => return,
                // etc...
                else => {},
            }
        }
    }

    fn reportParseError(token: Token) void {
        print("Error at {d}:{d} : {s}, {any} | ", .{token.span.line, token.span.start_col, token.lexeme, token.token_type});
    }

    fn collectError(self: *Parser, err_msg: ?[]const u8, span: Span) !void {
        const diag = Diagnostic{
            .msg = err_msg,
            .span = span,
            .severity = .err,  
        };
        try self.diagnostics.append(self.alloc, diag);
    }
};
