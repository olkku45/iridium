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

// TODO: partial nodes
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

    pub const ErrorNode = struct {};
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
        args: Expr,

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
        //annotation: ?TypeAnnotation,
    };
};

pub const LiteralType = enum {
    identifier,
    int,
    float,
    boolean,
    string,
};

// TODO: fix error messages' info being after the synchronization for some reason
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

// TODO: more general handling of identifiers
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
            if (stmt != null) try self.statements.append(self.alloc, stmt.?);
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
        var stmt: ?Stmt = undefined;

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
            else => {
                //reportParseError(currToken(self));
                //return ParseError.NotAStatement;
            },
        }
        if (stmt == null) return null;

        return stmt;
    }

    fn parseBlock(self: *Parser) !?[]Stmt {
        var stmt_list: std.array_list.Aligned(Stmt, null) = .empty;

        // TODO: do we have to synchronize here?
        if (try advance(self, .LEFT_BRACE, "expected '{'") == null) return null;

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
            if (try advance(self, .RIGHT_BRACE, "expected '}'") == null) return null;
        }

        const slice = try stmt_list.toOwnedSlice(self.alloc);
        return slice;
    }

    fn fnDeclaration(self: *Parser) !?Stmt {
        if (try advance(self, .FN, null) == null) return null;

        const func_name = try parseLiteral(self);
        if (func_name == null) return null;

        if (try advance(self, .LEFT_PAREN, "expected '(' after function name") == null) return null;

        // no params for now

        if (try advance(self, .RIGHT_PAREN, "expected ')'") == null) return null;
        if (try advance(self, .RIGHT_ARROW, "expected '->'") == null) return null; // TODO change to '=>'

        const ret_type = try parseType(self);
        const func_body = try parseBlock(self);
        if (ret_type == null or func_body == null) return null;

        // TODO: loop over stmts to see if there's a return stmt,
        // if not return error except if ret type is void, or some other way to check for a
        // return statement...
        
        return Stmt{ .fn_decl = .{
            .fn_body = func_body.?,
            .name = func_name.?,
            .ret_type = TypeAnnotation{
                .named = .{
                    .primitive = ret_type.?,
                },
            },
            .symbol = null,
        }};
    }

    fn ifStmt(self: *Parser) !?Stmt {
        if (try advance(self, .IF, "expected 'if'") == null) return null;
        if (try advance(self, .LEFT_PAREN, "expected '(' after if") == null) return null;

        // just a binary expression as condition for now
        // TODO: generalize for all expression types
        const cond = try binaryExpr(self);

        if (try advance(self, .RIGHT_PAREN, "expected ')' after condition") == null) return null;

        const if_body = try parseBlock(self);

        if (cond == null or if_body == null) return null;

        return Stmt{ .if_stmt = .{
            .condition = cond.?,
            .if_body = if_body.?,
        }};
    }

    fn binaryExpr(self: *Parser) !?Expr {
        const left = try parseLiteral(self);
        const op = try parseBinOperator(self);
        const right = try parseLiteral(self);

        if (left == null or op == null or right == null) return null;

        const binary = try self.alloc.create(Expr.BinaryExpr);
        binary.* = .{
            .left = left.?,
            .op = op.?,
            .right = right.?,
        };

        return Expr{ .binary = binary };
    }

    fn retStmt(self: *Parser) !?Stmt {
        if (try advance(self, .RETURN, "expected 'return'") == null) return null;
        
        const ret_value = try parseLiteral(self);
        if (ret_value == null) return null;

        if (try advance(self, .SEMICOLON, "expected ';' as line break") == null) return null;

        return Stmt{ .ret_stmt = .{
            .value = ret_value.?,
        }};
    }

    fn functionCall(self: *Parser) !?Stmt {
        const ident = try parseLiteral(self);

        if (try advance(self, .LEFT_PAREN, "expected '('") == null) return null;

        const arg = try parseLiteral(self); // just one arg right now

        if (try advance(self, .RIGHT_PAREN, "expected ')'") == null) return null;
        if (try advance(self, .SEMICOLON, "expected ';'") == null) return null;

        if (ident == null or arg == null) return null;

        const call = try self.alloc.create(Expr.CallExpr);
        call.* = .{
            .func_name = ident.?,
            .args = arg.?,
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
    
    fn variableDecl(self: *Parser) !?Stmt {
        var mutable = false;

        if (try advance(self, .LET, null) == null) return null;
        
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            if (try advance(self, .MUT, null) == null) return null;
        }

        const var_name = try parseLiteral(self);
        if (try advance(self, .COLON, "expected ':'") == null) return null;
        
        const var_type = try parseType(self);

        if (try advance(self, .EQUAL, "expected '='") == null) return null;

        const value = try parseLiteral(self);

        if (try advance(self, .SEMICOLON, "expected ';'") == null) return null;

        if (var_name == null or var_type == null or value == null) return null;

        return Stmt{ .var_decl = .{
            .mutable = mutable,
            .name = var_name.?,
            .value = Expr{ .literal = value.?.literal },
            .var_type = TypeAnnotation{ .named = .{
                .primitive = var_type.?,
            }},
            .symbol = null,
        }};
    }

    fn externFnDeclaration(self: *Parser) !?Stmt {
        if (try advance(self, .EXTERN, null) == null) return null;
        if (try advance(self, .FN, "expected 'fn'") == null) return null;

        const lit = try parseLiteral(self);
        
        if (try advance(self, .LEFT_PAREN, "expected ')'") == null) return null;
        if (try advance(self, .IDENTIFIER, null) == null) return null;
        if (try advance(self, .COLON, "expected ':'") == null) return null;

        const arg_type = try parseType(self);
        
        if (try advance(self, .RIGHT_PAREN, "expected ')'") == null) return null;
        if (try advance(self, .RIGHT_ARROW, "expected '->'") == null) return null; // TODO change to '=>'

        const ret_type = try parseType(self);

        if (try advance(self, .SEMICOLON, "expected ';'") == null) return null;

        // TODO can these be replaced with 'orelse'?
        if (lit == null or arg_type == null or ret_type == null) return null;

        return Stmt{ .extern_fn_decl = .{
            .name = lit.?,
            .arg_type = TypeAnnotation{ .named = .{
                .primitive = arg_type.?,
            }},
            .ret_type = TypeAnnotation{ .named = .{
                .primitive = ret_type.?,
            }},
            .symbol = null,
        }};
    }

    fn parseBinOperator(self: *Parser) !?BinaryOp {
        const op = currToken(self);
        const m = "expected operator";

        switch (op.token_type) {
            .PLUS => {
                if (try advance(self, .PLUS, m) == null) return null;
                return .ADD;   
            },
            .MINUS => {
                if (try advance(self, .MINUS, m) == null) return null;
                return .SUB;
            },
            .STAR => {
                if (try advance(self, .STAR, m) == null) return null;
                return .MUL;
            },
            .SLASH => {
                if (try advance(self, .SLASH, m) == null) return null;
                return .DIV;  
            },
            .GREATER => {
                if (try advance(self, .GREATER, m) == null) return null;
                return .GREATER;
            },
            .LESS => {
                if (try advance(self, .LESS, m) == null) return null;
                return .LESS;
            },
            .EQUAL => {
                if (try advance(self, .EQUAL, m) == null) return null;
                return .EQUAL;
            },
            .BANG_EQUAL => {
                if (try advance(self, .BANG_EQUAL, m) == null) return null;
                return .NOT_EQUAL;
            },
            .EQUAL_EQUAL => {
                if (try advance(self, .EQUAL_EQUAL, m) == null) return null;
                return .EQUAL_EQUAL;
            },
            .LESS_EQUAL => {
                if (try advance(self, .LESS_EQUAL, m) == null) return null;
                return .LESS_EQUAL;
            },
            .GREATER_EQUAL => {
                if (try advance(self, .GREATER_EQUAL, m) == null) return null;
                return .GREATER_EQUAL;
            },
            .PLUS_EQUAL => {
                if (try advance(self, .PLUS_EQUAL, m) == null) return null;
                return .ADD_EQUAL;
            },
            .MINUS_EQUAL => {
                if (try advance(self, .MINUS_EQUAL, m) == null) return null;
                return .SUB_EQUAL;
            },
            .STAR_EQUAL => {
                if (try advance(self, .STAR_EQUAL, m) == null) return null;
                return .MUL_EQUAL;
            },
            .SLASH_EQUAL => {
                if (try advance(self, .SLASH_EQUAL, m) == null) return null;
                return .DIV_EQUAL;
            },
            else => {
                try collectError(self, "expected an operator", try previousToken(self));
                if (try advanceSync(self) == null) return null;
            },
        }
        return null;
    }

    fn parseType(self: *Parser) !?TypeAnnotation.NamedType.PrimitiveType {
        const curr = currToken(self);
        const m = "expected a type";

        switch (curr.token_type) {
            .C_INT => {
                if (try advance(self, .C_INT, m) == null) return null;
                return .c_int;
            },
            .UINT8 => {
                if (try advance(self, .UINT8, m) == null) return null;
                return .u8;
            },
            .UINT16 => {
                if (try advance(self, .UINT16, m) == null) return null;
                return .u16;
            },
            .UINT32 => {
                if (try advance(self, .UINT32, m) == null) return null;
                return .u32;  
            },
            .UINT64 => {
                if (try advance(self, .UINT64, m) == null) return null;
                return .u64;  
            },
            .INT8 => {
                if (try advance(self, .INT8, m) == null) return null;
                return .i8;  
            },
            .INT16 => {
                if (try advance(self, .INT16, m) == null) return null;
                return .i16;  
            },
            .INT32 => {
                if (try advance(self, .INT32, m) == null) return null;
                return .i32;  
            },
            .INT64 => {
                if (try advance(self, .INT64, m) == null) return null;
                return .i64;
            },
            .FLOAT32 => {
                if (try advance(self, .FLOAT32, m) == null) return null;
                return .f32;  
            },
            .FLOAT64 => {
                if (try advance(self, .FLOAT64, m) == null) return null;
                return .f64;  
            },
            .BOOL => {
                if (try advance(self, .BOOL, m) == null) return null;
                return .bool;  
            },
            .VOID => {
                if (try advance(self, .VOID, m) == null) return null;
                return .void;
            },
            else => {
                // TODO see if taking previous token causes further issues, currently
                // it fixes some related to wrong error msgs
                try collectError(self, "expected a type", try previousToken(self));
                if (try synchronize(self) == null) return null;
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
                if (try advance(self, .IDENTIFIER, "expected an identifier") == null) return null;
            },
            .INTEGER => {
                lit_type = .int;
                if (try advance(self, .INTEGER, "expected an integer") == null) return null;
            },
            .FLOAT => {
                lit_type = .float;
                if (try advance(self, .FLOAT, "expected a float") == null) return null;  
            },
            .BOOL => {
                lit_type = .boolean;
                if (try advance(self, .BOOL, "expected a boolean") == null) return null;
            },
            .STRING => {
                lit_type = .string;
                if (try advance(self, .STRING, "expected a string") == null) return null;  
            },
            .CHARACTER => {
                lit_type = .int;
                if (try advance(self, .CHARACTER, "expected a character") == null) return null;  
            },
            else => {
                try collectError(self, "expected a literal", try previousToken(self));
                if (try advanceSync(self) == null) return null;
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
            if (try synchronize(self) == null) return null;
            return null;
        }
        if (token_type.category() == .KEYWORD) {
            if (try checkKeyword(self, currToken(self)) == null) return null;
        }
        self.current += 1;
    }

    // TODO is this a misleading name?
    fn advanceSync(self: *Parser) !?void {
        if (isAtEnd(self)) {
            // TODO: see the todo in advance()
            //try collectError(self, "unexpected end of file", currToken(self).span);
            return null;
        }
        self.current += 1;
    }

    fn checkKeyword(self: *Parser, token: Token) !?void {
        switch (token.token_type) {
            .EXTERN => {
                const next = try nextToken(self);
                if (next.token_type != .FN) {
                    try collectError(self, "expected 'fn'", try previousToken(self));
                    if (try synchronize(self) == null) return null;
                    return null;
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

    fn indexFault(self: *Parser) bool {
        return self.current > self.tokens.len - 1;
    }

    // find a 'safe' token
    fn synchronize(self: *Parser) anyerror!?void {
        while (!isAtEnd(self)) {
            if (getCurrentTokenType(self) == .SEMICOLON) {
                if (try advance(self, .SEMICOLON, null) == null) return null;
                return;
            }

            switch (getCurrentTokenType(self)) {
                .FN => return,
                .FOR => return,
                .WHILE => return,
                .IF => return,
                .RETURN => return,
                .LET => return,
                //.LEFT_BRACE => return,
                //.RIGHT_BRACE => return,
                .EXTERN => return,
                // etc...
                else => {},
            }
            // can only happen at the end of a file
            if (try advanceSync(self) == null) return null;
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

        //const err = Stmt{ .error_node = .{ .span = span } };
        //try self.statements.append(self.alloc, err);
    }
};
