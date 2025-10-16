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
};

pub const Error = struct {
    
};

pub const Span = struct {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    source_file: []const u8,  
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
    if_stmt: *IfStmt,
    extern_fn_decl: *ExternFnDecl,
    fn_decl: *FnDecl,
    var_decl: *VariableDecl,
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
        //params:
        //ret_type:

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
        }
    }

    fn externFnDeclaration(self: *Parser) Stmt {
        
    }

    fn advanceKeyword(self: *Parser, token: Token) !void {
        if (token.token_type != TokenType.Keyword) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        checkKeyword(self, token);
        self.current += 1;
    }

    fn advanceIdentifier(self: *Parser, token: Token) !void {
        if (token.token_type != TokenType.Special) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceType(self: *Parser, token: Token) !void {
        if (token.token_type != TokenType.Type) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceOperator(self: *Parser, token: Token) !void {
        if (token.token_type != TokenType.Operator) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn advanceCharacter(self: *Parser, token: Token) !void {
        if (token.token_type != TokenType.Char) {
            reportParseError(token);
            return ParseError.WrongToken;
        }
        self.current += 1;
    }

    fn checkKeyword(self: *Parser, token: Token) !void {
        switch (token.token_type.Keyword) {
            .EXTERN => {
                if (nextToken(self).token_type != TokenType.Keyword.FN) {
                    reportParseError(token);
                    return ParseError.WrongToken;
                }
            }
        }
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
        return self.tokens[self.current];
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
