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
    void_node: VoidNode,

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
        // something here?
    };

    pub const VoidNode = struct {};
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

pub const Parser = struct {
    tokens: []Token,
    current: usize,
    alloc: std.mem.Allocator,
    diagnostics: std.array_list.Aligned(Diagnostic, null),

    pub fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .alloc = allocator,
            .diagnostics = initDiagnostics(),
        };
    }

    pub fn parseTokens(self: *Parser) ![]Stmt {
        var statements: std.array_list.Aligned(Stmt, null) = .empty;

        while (!isAtEnd(self)) {
            const stmt = try parseStatement(self);
            try statements.append(self.alloc, stmt);
        }
        
        const slice = try statements.toOwnedSlice(self.alloc);
        
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
            if (isAtEnd(self)) return ParseError.UnexpectedEOF; // TODO change this

            const stmt = try parseStatement(self); // TODO change how this advances
            try stmt_list.append(self.alloc, stmt);
        }

        try advance(self, .RIGHT_BRACE, "expected '}'");

        const slice = stmt_list.toOwnedSlice(self.alloc);
        return slice;
    }

    fn fnDeclaration(self: *Parser) !Stmt {
        try advance(self, .FN, null);

        try self.expect(.IDENTIFIER);
        const func_name = try parseLiteral(self);

        try advance(self, .LEFT_PAREN, "expected '(' after function name");

        // no args for now

        try advance(self, .RIGHT_PAREN, "expected ')'");
        try advance(self, .RIGHT_ARROW, "expected '->'");

        const ret_type = try parseType(self);
        try advanceType(self, currToken(self));

        const func_body = try parseBlock(self);

        // TODO: loop over stmts to see if there's a return stmt,
        // if not return error, or some other way to check for a
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
        // TODO: generalize for all expressions
        const cond = try binaryExpr(self);

        try advance(self, .RIGHT_PAREN, "expected ')' after condition");

        const if_body = try parseBlock(self);

        return Stmt{ .if_stmt = .{
            .condition = cond,
            .if_body = if_body,
        }};
    }

    fn binaryExpr(self: *Parser) !Expr {
        const left = try parseLiteral(self);

        const op = currToken(self);
        try advanceOperator(self, currToken(self));

        var bin_op: BinaryOp = undefined;
        switch (op.token_type) {
            .PLUS => { bin_op = .ADD; },
            .MINUS => { bin_op = .SUB; },
            .STAR => { bin_op = .MUL; },
            .SLASH => { bin_op = .DIV; },
            .GREATER => { bin_op = .GREATER; },
            .LESS => { bin_op = .LESS; },
            .EQUAL => { bin_op = .EQUAL; },
            .BANG_EQUAL => { bin_op = .NOT_EQUAL; },
            .EQUAL_EQUAL => { bin_op = .EQUAL_EQUAL; },
            .LESS_EQUAL => { bin_op = .LESS_EQUAL; },
            .GREATER_EQUAL => { bin_op = .GREATER_EQUAL; },
            .PLUS_EQUAL => { bin_op = .ADD_EQUAL; },
            .MINUS_EQUAL => { bin_op = .SUB_EQUAL; },
            .STAR_EQUAL => { bin_op = .MUL_EQUAL; },
            .SLASH_EQUAL => { bin_op = .DIV_EQUAL; },
            else => {},
        }

        const right = try parseLiteral(self);

        const binary = try self.alloc.create(Expr.BinaryExpr);
        binary.* = .{
            .left = left,
            .op = bin_op,
            .right = right,
        };

        return Expr{ .binary = binary };
    }

    fn retStmt(self: *Parser) !Stmt {
        try advance(self, .RETURN, null);
        
        const ret_value = try literal(self);

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
                    const str_arg = try literalWithType(self, .primitive);
                    try args.append(self.alloc, str_arg);
                },
                .IDENTIFIER => {
                    const ident_arg = try literalWithType(self, .variable);
                    try args.append(self.alloc, ident_arg);  
                },
                .CHARACTER => {
                    const char_arg = try literalWithType(self, .primitive);
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
            // TODO: shorten this
            .func_name = Expr{ .literal = .{
                .value = ident.literal.value,
                .span = ident.literal.span,
                .type = .function,
                .annotation = null,
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
        const line = currToken(self).span.line;

        try advance(self, .LET, null);
        
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            try advance(self, .MUT, null);
        }

        const var_name = try parseLiteral(self);
        try advance(self, .COLON, "expected ':'");
        
        const var_type = try parseType(self);
        try advanceType(self, currToken(self));

        try advance(self, .EQUAL, "expected '='");

        const value = try parseLiteral(self);

        // multi line var decl currently not allowed (kind of arbitrary though)
        if (currToken(self).span.line != line) {
            reportParseError(currToken(self));
            return ParseError.MultiLineVarDecl;
        }

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

        try advanceIdentifier(self, currToken(self));

        try advance(self, .COLON, "expected ':'");

        const arg_type = try parseType(self);
        try advanceType(self, currToken(self));
        
        try advance(self, .RIGHT_PAREN, "expected ')'");
        try advance(self, .RIGHT_ARROW, "expected '->'");

        const ret_type = try parseType(self);
        try advanceType(self, currToken(self));

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

    fn parseType(self: *Parser) ?TypeAnnotation.NamedType.PrimitiveType {
        const curr = currToken(self);

        switch (curr.token_type) {
            .C_INT => {
                advance(self, .C_INT, null);
                return .c_int;
            },
            .UINT8 => {
                advance(self, .UINT8, null);
                return .u8;
            },
            .UINT16 => {
                advance(self, .UINT16, null);
                return .u16;
            },
            .UINT32 => {
                advance(self, .UINT32, null);
                return .u32;  
            },
            .UINT64 => {
                advance(self, .UINT64, null);
                return .u64;  
            },
            .INT8 => {
                advance(self, .INT8, null);
                return .i8;  
            },
            .INT16 => {
                advance(self, .INT16, null);
                return .i16;  
            },
            .INT32 => {
                advance(self, .INT32, null);
                return .i32;  
            },
            .INT64 => {
                advance(self, .INT64, null);
                return .i64;
            },
            .FLOAT32 => {
                advance(self, .FLOAT32, null);
                return .f32;  
            },
            .FLOAT64 => {
                advance(self, .FLOAT64, null);
                return .f64;  
            },
            .BOOL => {
                advance(self, .BOOL, null);
                return .bool;  
            },
            .VOID => {
                advance(self, .VOID, null);
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

    // TODO return error node
    fn advance(self: *Parser, token_type: TokenType, msg: ?[]const u8) !void {
        if (isAtEnd(self)) {
            try collectError(self, "Unexpected end of file", currToken(self).span);
        }
        if (getCurrentTokenType(self) != token_type) {
            try collectError(self, msg, currToken(self).span);
        }
        if (token_type.category() == .KEYWORD) {
            try checkKeyword(self, currToken(self));
        }
        self.current += 1;
    }

    // TODO: remove all of these different advance functions
    fn advanceKeyword(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;

        if (token.token_type.category() != .KEYWORD) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        try checkKeyword(self, token);
        self.current += 1;
    }

    fn advanceIdentifier(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
        
        if (token.token_type.category() != .IDENTIFIER) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceType(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
            
        if (token.token_type.category() != .TYPE) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceOperator(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
        
        if (token.token_type.category() != .OPERATOR) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceCharacter(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
        
        if (token.token_type.category() != .CHARACTER) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    // TODO remove this also
    fn checkKeyword(self: *Parser, token: Token) !void {
        switch (token.token_type) {
            .EXTERN => {
                const next = try nextToken(self);
                if (next.token_type != .FN) {
                    reportParseError(token);
                    return ParseError.WrongToken;
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
