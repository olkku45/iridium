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
    while_loop: WhileLoop,
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
                return try functionCall(self) orelse return null;    
            },
            .RETURN => {
                return try retStmt(self) orelse return null;
            },
            .WHILE => {
                return try whileLoop(self) orelse return null;
            },
            else => {
                //reportParseError(currToken(self));
                //return ParseError.NotAStatement;
            },
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

    fn whileLoop(self: *Parser) !?Stmt {
       try advance(self, .WHILE, null) orelse return null;
       try advance(self, .LEFT_PAREN, "expected '('") orelse return null;

       // TODO generalize the condition here
       const cond = try parseLiteral(self) orelse return null;

       try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;

       const body = try parseBlock(self) orelse return null;

       return Stmt{ .while_loop = .{
           .cond = cond,
           .body = body,
       }};
    }

    fn fnDeclaration(self: *Parser) !?Stmt {
        try advance(self, .FN, null) orelse return null;

        const func_name = try parseLiteral(self) orelse return null;

        try advance(self, .LEFT_PAREN, "expected '(' after function name") orelse return null;

        // no params for now

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
            .ret_type = TypeAnnotation{
                .named = .{
                    .primitive = ret_type,
                },
            },
            .symbol = null,
        }};
    }

    fn ifStmt(self: *Parser) !?Stmt {
        try advance(self, .IF, "expected 'if'") orelse return null;
        try advance(self, .LEFT_PAREN, "expected '(' after if") orelse return null;

        // just a binary expression as condition for now
        // TODO: generalize for all expression types
        const cond = try binaryExpr(self) orelse return null;

        try advance(self, .RIGHT_PAREN, "expected ')' after condition") orelse return null;

        const if_body = try parseBlock(self) orelse return null;

        return Stmt{ .if_stmt = .{
            .condition = cond,
            .if_body = if_body,
        }};
    }

    fn binaryExpr(self: *Parser) !?Expr {
        const left = try parseLiteral(self) orelse return null;
        const op = try parseBinOperator(self) orelse return null;
        const right = try parseLiteral(self) orelse return null;

        const binary = try self.alloc.create(Expr.BinaryExpr);
        binary.* = .{
            .left = left,
            .op = op,
            .right = right,
        };

        return Expr{ .binary = binary };
    }

    fn retStmt(self: *Parser) !?Stmt {
        try advance(self, .RETURN, "expected 'return'") orelse return null;
        
        const ret_value = try parseLiteral(self) orelse return null;
        
        try advance(self, .SEMICOLON, "expected ';' as line break") orelse return null;

        return Stmt{ .ret_stmt = .{
            .value = ret_value,
        }};
    }

    fn functionCall(self: *Parser) !?Stmt {
        const ident = try parseLiteral(self) orelse return null;

        try advance(self, .LEFT_PAREN, "expected '('") orelse return null;

        const arg = try parseLiteral(self) orelse return null; // just one arg right now

        try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;
        try advance(self, .SEMICOLON, "expected ';'") orelse return null;

        const call = try self.alloc.create(Expr.CallExpr);
        call.* = .{
            .func_name = ident,
            .args = arg,
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

        try advance(self, .LET, null) orelse return null;
        
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            try advance(self, .MUT, null) orelse return null;
        }

        const var_name = try parseLiteral(self) orelse return null;
        try advance(self, .COLON, "expected ':'") orelse return null;
        
        const var_type = try parseType(self) orelse return null;

        try advance(self, .EQUAL, "expected '='") orelse return null;

        const value = try parseLiteral(self) orelse return null;

        try advance(self, .SEMICOLON, "expected ';'") orelse return null;

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

    fn externFnDeclaration(self: *Parser) !?Stmt {
        try advance(self, .EXTERN, null) orelse return null;
        try advance(self, .FN, "expected 'fn'") orelse return null;

        const lit = try parseLiteral(self) orelse return null;
        
        try advance(self, .LEFT_PAREN, "expected '('") orelse return null;
        try advance(self, .IDENTIFIER, null) orelse return null;
        try advance(self, .COLON, "expected ':'") orelse return null;

        const arg_type = try parseType(self) orelse return null;
        
        try advance(self, .RIGHT_PAREN, "expected ')'") orelse return null;
        try advance(self, .RIGHT_ARROW, "expected '=>'") orelse return null;

        const ret_type = try parseType(self) orelse return null;

        try advance(self, .SEMICOLON, "expected ';'") orelse return null;

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
                try advanceSync(self) orelse return null;
            },
        }
        return null;
    }

    fn parseType(self: *Parser) !?TypeAnnotation.NamedType.PrimitiveType {
        const curr = currToken(self);
        const m = "expected a type";

        switch (curr.token_type) {
            .C_INT => {
                try advance(self, .C_INT, m) orelse return null;
                return .c_int;
            },
            .UINT8 => {
                try advance(self, .UINT8, m) orelse return null;
                return .u8;
            },
            .UINT16 => {
                try advance(self, .UINT16, m) orelse return null;
                return .u16;
            },
            .UINT32 => {
                try advance(self, .UINT32, m) orelse return null;
                return .u32;  
            },
            .UINT64 => {
                try advance(self, .UINT64, m) orelse return null;
                return .u64;  
            },
            .INT8 => {
                try advance(self, .INT8, m) orelse return null;
                return .i8;  
            },
            .INT16 => {
                try advance(self, .INT16, m) orelse return null;
                return .i16;  
            },
            .INT32 => {
                try advance(self, .INT32, m) orelse return null;
                return .i32;  
            },
            .INT64 => {
                try advance(self, .INT64, m) orelse return null;
                return .i64;
            },
            .FLOAT32 => {
                try advance(self, .FLOAT32, m) orelse return null;
                return .f32;  
            },
            .FLOAT64 => {
                try advance(self, .FLOAT64, m) orelse return null;
                return .f64;  
            },
            .BOOL => {
                try advance(self, .BOOL, m) orelse return null;
                return .bool;  
            },
            .VOID => {
                try advance(self, .VOID, m) orelse return null;
                return .void;
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

    // TODO this isn't doing at all what I wanted! fix!
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
            .BOOL => {
                lit_type = .boolean;
                try advance(self, .BOOL, "expected a boolean") orelse return null;
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
                try advanceSync(self) orelse return null;
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

    // TODO is this a misleading name?
    fn advanceSync(self: *Parser) !?void {
        if (isAtEnd(self)) {
            // TODO: see the todo in advance()
            //try collectError(self, "unexpected end of file", currToken(self).span);
            return null;
        }
        self.current += 1;
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
