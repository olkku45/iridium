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
        type: LiteralType,
        annotation: ?TypeAnnotation,
    };
};

pub const LiteralType = enum {
    variable,
    function,
    primitive,
};

pub const Parser = struct {
    tokens: []Token,
    current: usize,
    alloc: std.mem.Allocator,

    pub fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .alloc = allocator,
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

        try self.expect(.LEFT_BRACE);
        try advanceCharacter(self, currToken(self));

        while (getCurrentTokenType(self) != .RIGHT_BRACE) {
            if (isAtEnd(self)) return ParseError.UnexpectedEOF;

            const stmt = try parseStatement(self);
            try stmt_list.append(self.alloc, stmt);
        }

        try self.expect(.RIGHT_BRACE);
        try advanceCharacter(self, currToken(self));

        const slice = stmt_list.toOwnedSlice(self.alloc);
        return slice;
    }

    fn fnDeclaration(self: *Parser) !Stmt {
        try self.expect(.FN);
        try advanceKeyword(self, currToken(self));

        try self.expect(.IDENTIFIER);
        const func_name = try literalWithType(self, .function);

        // no args for now
        try self.expect(.LEFT_PAREN);
        try advanceCharacter(self, currToken(self));
        try self.expect(.RIGHT_PAREN);
        try advanceCharacter(self, currToken(self));

        try self.expect(.RIGHT_ARROW);
        try advanceOperator(self, currToken(self));

        const ret_type = try getType(self);
        try advanceType(self, currToken(self));

        const func_body = try parseBlock(self);

        // TODO: loop over stmts to see if there's a return stmt,
        // if not return error, or some other way to check for a
        // return statement...
        
        return Stmt{ .fn_decl = .{
            .fn_body = func_body,
            .name = func_name,
            .ret_type = ret_type,
            .symbol = null,
        }};
    }

    fn ifStmt(self: *Parser) !Stmt {
        try self.expect(.IF);
        try advanceKeyword(self, currToken(self));

        try self.expect(.LEFT_PAREN);
        try advanceCharacter(self, currToken(self));

        // just a binary expression as condition for now
        const cond = try binaryExpr(self);

        try self.expect(.RIGHT_PAREN);
        try advanceCharacter(self, currToken(self));

        const if_body = try parseBlock(self);

        return Stmt{ .if_stmt = .{
            .condition = cond,
            .if_body = if_body,
        }};
    }

    fn binaryExpr(self: *Parser) !Expr {
        const left = try literal(self);

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

        const right = try literal(self);

        const binary = try self.alloc.create(Expr.BinaryExpr);
        binary.* = .{
            .left = left,
            .op = bin_op,
            .right = right,
        };

        return Expr{ .binary = binary };
    }

    fn retStmt(self: *Parser) !Stmt {
        try self.expect(.RETURN);
        try advanceKeyword(self, currToken(self));
        
        const ret_value = try literal(self);

        try self.expect(.SEMICOLON);
        try advanceCharacter(self, currToken(self));

        return Stmt{ .ret_stmt = .{
            .value = ret_value,
        }};
    }

    fn functionCall(self: *Parser) !Stmt {
        const ident = try literalWithType(self, .function);
        
        try self.expect(.LEFT_PAREN);
        try advanceCharacter(self, currToken(self));

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

        try self.expect(.RIGHT_PAREN);
        try advanceCharacter(self, currToken(self));

        try self.expect(.SEMICOLON);
        try advanceCharacter(self, currToken(self));

        const call = try self.alloc.create(Expr.CallExpr);
        call.* = .{
            // TODO: shorten this
            .func_name = Expr{ .literal = .{
                .value = ident.literal.value,
                .span = Span{
                    .line = ident.literal.span.line,
                    .start_col = ident.literal.span.start_col,
                    .end_col = ident.literal.span.end_col,
                    .source_file = ident.literal.span.source_file,
                },
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

        try self.expect(.LET);        
        try advanceKeyword(self, currToken(self));
        
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            try advanceKeyword(self, currToken(self));
        }

        const var_name = try literalWithType(self, .variable);

        try self.expect(.COLON);
        try advanceCharacter(self, currToken(self));
        
        const var_type = try getType(self);
        try advanceType(self, currToken(self));

        try self.expect(.EQUAL);
        try advanceOperator(self, currToken(self));

        // currently support only literals for variable assignments
        const value = try literal(self);

        // multi line var decl currently not allowed (kind of arbitrary though)
        if (currToken(self).span.line != line) {
            reportParseError(currToken(self));
            return ParseError.MultiLineVarDecl;
        }

        try self.expect(.SEMICOLON);
        try advanceCharacter(self, currToken(self));

        return Stmt{ .var_decl = .{
            .mutable = mutable,
            .name = var_name,
            .value = Expr{ .literal = value.literal },
            .var_type = var_type,
            .symbol = null,
        }};
    }

    fn externFnDeclaration(self: *Parser) !Stmt {
        try self.expect(.EXTERN);
        try advanceKeyword(self, currToken(self));
        try self.expect(.FN);
        try advanceKeyword(self, currToken(self));

        const lit = try literalWithType(self, .function);

        try self.expect(.LEFT_PAREN);
        try advanceCharacter(self, currToken(self));

        try advanceIdentifier(self, currToken(self));
        try self.expect(.COLON);
        try advanceCharacter(self, currToken(self));

        const arg_type = try getType(self);
        try advanceType(self, currToken(self));
        
        try self.expect(.RIGHT_PAREN);
        try advanceCharacter(self, currToken(self));

        try self.expect(.RIGHT_ARROW);
        try advanceOperator(self, currToken(self));

        const ret_type = try getType(self);
        try advanceType(self, currToken(self));

        try self.expect(.SEMICOLON);
        try advanceCharacter(self, currToken(self));

        return Stmt{ .extern_fn_decl = .{
            .name = lit,
            .arg_type = arg_type,
            .ret_type = ret_type,
            .symbol = null,
        }};
    }

    fn getType(self: *Parser) !TypeAnnotation {
        const curr = currToken(self);
        if (curr.token_type.category() != .TYPE) {
            reportParseError(curr);
            return ParseError.WrongToken;
        }

        var curr_type: TypeAnnotation = undefined;
        switch (curr.token_type) {
            .C_INT => {
                curr_type = TypeAnnotation { .named = .{
                    .primitive = .c_int,
                }};
            },
            // etc...
            else => {},
        }

        return curr_type;
    }

    fn getTypeAnnotation(token_type: TokenType) TypeAnnotation {
        var at: TypeAnnotation.NamedType.PrimitiveType = undefined;
        
        switch (token_type) {
            .UINT8 => { at = .u8; },
            .UINT16 => { at = .u16; },
            .UINT32 => { at = .u32; },
            .UINT64 => { at = .u64; },
            .INT8 => { at = .i8; },
            .INT16 => { at = .i16; },
            .INT32 => { at = .i32; },
            .INT64 => { at = .i64; },
            .FLOAT32 => { at = .f32; },
            .FLOAT64 => { at = .f64; },
            .BOOL => { at = .bool; },
            .VOID => { at = .void; },
            .C_INT => { at = .c_int; },
            else => {},
        }

        return TypeAnnotation { .named = .{
            .primitive = at,
        }};
    }

    fn literalWithType(self: *Parser, lit_type: LiteralType) !Expr {
        const curr = currToken(self);

        var annotation: TypeAnnotation = undefined;
        
        if (curr.token_type.category() == .IDENTIFIER) {
            try advanceIdentifier(self, currToken(self));
        } else if (curr.token_type.category() == .TYPE) {
            annotation = getTypeAnnotation(curr.token_type);
            try advanceType(self, currToken(self));
        } else {
            reportParseError(curr);
            return ParseError.WrongTokenType;
        }

        const lit = Expr{ .literal = .{
            .value = curr.lexeme,
            .span = Span{
                .start_col = curr.span.start_col,
                .end_col = curr.span.end_col,
                .line = curr.span.line,
                .source_file = null,
            },
            .type = lit_type,
            .annotation = annotation,
        }};

        return lit;
    }

    fn literal(self: *Parser) !Expr {
        const curr = currToken(self);

        var lit_type: LiteralType = undefined;
        var annotation: TypeAnnotation = undefined;
        
        if (curr.token_type.category() == .IDENTIFIER) {
            lit_type = .variable;
            try advanceIdentifier(self, currToken(self));
        } else if (curr.token_type.category() == .TYPE) {
            lit_type = .primitive;
            annotation = getTypeAnnotation(curr.token_type);
            try advanceType(self, currToken(self));
        } else {
            reportParseError(curr);
            return ParseError.WrongTokenType;
        }

        const lit = Expr{ .literal = .{
            .value = curr.lexeme,
            .span = Span{
                .start_col = curr.span.start_col,
                .end_col = curr.span.end_col,
                .line = curr.span.line,
                .source_file = null,
            },
            .type = lit_type,
            .annotation = annotation,
        }};

        return lit;
    }
    
    // TODO: collect errors into a list or something, so we can report
    // multiple errors at once
    fn expect(self: *Parser, token_type: TokenType) !void {
        const curr = currToken(self);    
        if (getCurrentTokenType(self) != token_type) {
            reportParseError(curr);
            return ParseError.WrongTokenType;
        }
    }
    
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
};
