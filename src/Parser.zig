// TODO:
// 1. break up Node union
// 2. have nodes store location info (token span) instead of full token

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Symbol = @import("Analyzer.zig").Symbol;
const SymbolTable = @import("Analyzer.zig").SymbolTable;

const Token = Tokenizer.Token;
const TokenType = Tokenizer.TokenType;

const print = std.debug.print;

const ParseError = error{
    WrongToken,
    MultiLineVarDecl,
};

pub const Error = struct {
    
};

pub const Span = struct {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    source_file: ?[]const u8,  
};

pub const BinaryOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
    EQUAL,
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

// do we need this?
pub const Type = union(enum) {
    u8,
    str: []const u8,
    void,
    bool,
    i32,
    u32,
    c_int,  
};

pub const Stmt = union(enum) {
    if_stmt: IfStmt,
    extern_fn_decl: ExternFnDecl,
    fn_decl: FnDecl,
    var_decl: VariableDecl,
    expr_stmt: Expr,

    pub const IfStmt = struct {
        condition: Expr,
        if_body: []Stmt,
        //else_body: ?[]Stmt,
    };

    pub const ExternFnDecl = struct {
        name: Expr,
        arg_type: Type,
        ret_type: Type,

        symbol: ?*Symbol,
    };

    pub const FnDecl = struct {
        name: Expr,
        fn_body: []Stmt,
        //params: , COMING SOON!!!
        ret_type: Type,

        symbol: ?*Symbol,
    };

    pub const VariableDecl = struct {
        name: Expr,
        value: Expr,
        mutable: bool,
        span: Span,
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
        left: Literal,
        op: BinaryOp,
        right: Literal,
    };

    pub const UnaryExpr = struct {
        op: UnaryOp,
        operand: Literal,
    };

    pub const CallExpr = struct {
        func_name: Literal,
        args: []Literal,

        func_symbol: ?*Symbol,
        ret_type: ?Type,
    };

    pub const VarUpdateExpr = struct {
        name: Literal,
        op: BinaryOp,
        value: Literal,

        var_symbol: ?*Symbol,
        var_type: ?Type,
    };
};

pub const Literal = union(enum) {
    bool: bool,
    char: u8,
    string: []const u8,
    null,
    float: f32,
    int: i32,
    uint: u32,
    void,
};

pub const AstPrinter = struct {
      
};

pub const Parser = struct {
    tokens: []Token,
    current: usize,
    alloc: std.mem.Allocator,

    pub fn create(tokens: []Token, allocator: std.mem.Allocator) Parser {
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

        return statements.toOwnedSlice(self.alloc);
    }

    fn parseStatement(self: *Parser) !Stmt {
        const curr_token_type = getCurrentTokenType(self);
        var stmt: Stmt = undefined;

        switch (curr_token_type) {
            .EXTERN => {
                stmt = try externFnDeclaration(self);
            },
            .FN => {
                stmt = try fnDeclaration(self);  
            },
            else => {},
        }
    }

    fn fnDeclaration(self: *Parser) ![]Stmt {
        var func_body: std.array_list.Aligned(Stmt, null) = .empty;

        advanceKeyword(self, currToken(self));
        const func_name = try literal(self);

        advanceCharacter(self, currToken(self));
        advanceCharacter(self, currToken(self));

        advanceOperator(self, currToken(self));
        const ret_type = getType(self);

        advanceCharacter(self, currToken(self));

        var curr_type = self.tokens[self.current].token_type;

        while (curr_type != .RETURN) {
            switch (curr_type) {
                .LET => {
                    const var_decl = try variableDecl(self);
                    try func_body.append(var_decl);
                },
                .IF => {
                    const if_stmt = try ifStmt(self);
                    try func_body.append(if_stmt);  
                },
                .IDENTIFIER => {
                    const call = try functionCall(self);
                    try func_body.append(call);
                },
                else => {},
            }
            curr_type = self.tokens[self.current].token_type;
        }

        return func_body.toOwnedSlice(self.alloc);
    }

    // TODO: variable declaration without value
    
    fn variableDecl(self: *Parser) !Stmt {
        var mutable = false;

        const start = currToken(self).col;
        const line = currToken(self).line;
        
        try advanceKeyword(self, currToken(self));
        if (getCurrentTokenType(self) == .MUT) {
            mutable = true;
            try advanceKeyword(self, currToken(self));
        }

        const name = try literal(self);
        
        if (getCurrentTokenType(self) != .COLON) {
            reportParseError(currToken(self));
            return ParseError.WrongToken; // TODO: add more errors
        }
        try advanceCharacter(self, currToken(self));
        
        const token_type = getCurrentTokenType(self);
        var var_type: Type = undefined;
        switch (token_type) {
            .C_INT => { var_type = .c_int; },
            // etc...
            else => {},
        }

        try advanceType(self, currToken(self));
        try advanceOperator(self, currToken(self));

        // currently support only literals for variable assignments
        const value = try literal(self);

        if (getCurrentTokenType(self) != .SEMICOLON) {
            reportParseError(currToken(self));
            return ParseError.WrongToken;
        }

        const end = currToken(self).col;

        if (currToken(self).line != line) {
            reportParseError(currToken(self));
            return ParseError.MultiLineVarDecl;
        }
        
        try advanceCharacter(self, currToken(self));

        return Stmt{ .var_decl = .{
            .mutable = mutable,
            .name = name,
            .value = value,
            .span = .{
                .start_col = start,
                .end_col = end,
                .start_line = line,
                .end_line = line,
                // null because currently we only have one file
                .source_file = null, 
            },
        }};
    }

    fn externFnDeclaration(self: *Parser) !Stmt {
        try advanceKeyword(self, currToken(self));
        try advanceKeyword(self, currToken(self));

        const ident = try literal(self);
        try advanceCharacter(self, currToken(self));

        const arg_type = try getType(self);
        try advanceCharacter(self, currToken(self));
        try advanceOperator(self, currToken(self));

        const ret_type = try getType(self);
        try advanceCharacter(self, currToken(self));

        return Stmt{ .extern_fn_decl = .{
            // WARNING: problem here!!!
            .name = ident,
            .arg_type = arg_type,
            .ret_type = ret_type,
            .symbol = null,
        }};
    }

    fn getType(self: *Parser) !Type {
        const curr = self.tokens[self.current];
        if (!TokenType.isType(curr.token_type)) {
            reportParseError(curr);
            return ParseError.WrongToken;
        }

        var curr_type: Type = undefined;
        switch (curr.token_type) {
            .C_INT => {
                curr_type = Type { .c_int };
            },
            else => {},
        }
        
        advanceType(self, curr);
        return curr_type;
    }

    fn literal(self: *Parser) !Literal {
        const name = self.tokens[self.current].lexeme;
        const ident = Literal{ .string = name };
    
        try advanceIdentifier(self, currToken(self));
        return ident;
    }

    fn advanceKeyword(self: *Parser, token: Token) !void {
        // right now just return early and don't advance
        if (isAtEnd(self)) return;
        
        if (!TokenType.isKeyword(token.token_type)) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        try checkKeyword(self, token);
        self.current += 1;
    }

    fn advanceIdentifier(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
        
        if (!TokenType.isIdentifier(token.token_type)) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceType(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
            
        if (!TokenType.isType(token.token_type)) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceOperator(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
        
        if (!TokenType.isOperator(token.token_type)) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceCharacter(self: *Parser, token: Token) !void {
        if (isAtEnd(self)) return;
        
        if (!TokenType.isCharacter(token.token_type)) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn checkKeyword(self: *Parser, token: Token) !void {
        switch (token.token_type) {
            .EXTERN => {
                if (nextToken(self).token_type != .FN) {
                    reportParseError(token);
                    return ParseError.WrongToken;
                }
            },
            // etc...
            else => {},
        }
    }

    fn currToken(self: *Parser) !Token {
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
        print("Error at {d}:{d} : {s} | ", .{token.line, token.col, token.lexeme});
    }
};
