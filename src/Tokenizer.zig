const std = @import("std");
const main = @import("main.zig");

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_BRACE,
    RIGHT_BRACE,
    
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    QUERY,
    COLON,
    
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL,

    RIGHT_ARROW,

    AND,
    OR,
    UINT8,
    UINT16,
    UINT32,
    UINT64,
    INT8,
    INT16,
    INT32,
    INT64,
    FLOAT32,
    FLOAT64,
    BOOL,
    VOID,
    INTEGER,
    FLOAT,
    IDENTIFIER,
    STRING,
    CHARACTER,
    C_INT,
    C_FLOAT,
    C_DOUBLE,
    C_CHAR,
    
    USE,
    LET,
    MUT,
    CONST,
    GLOBAL,
    FN,
    FOR,
    WHILE,
    RETURN,
    STRUCT,
    ENUM,
    UNION,
    CONTINUE,
    BREAK,
    MATCH,
    IF,
    ELSE,
    STD,
    LIB,
    EXTERN,
    CATCH,
    ASSERT,
    SUPPRESS,
    EXCLUDE,
    THROW,
    IN,
    AS,
    TRY,
    PUB,

    EOF
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
};

fn token(token_type: TokenType, lexeme: []const u8) Token {
    return Token{
        .token_type = token_type,
        .lexeme = lexeme,
    };
}

fn initTokens() std.array_list.Aligned(Token, null) {
    const tokens_list: std.array_list.Aligned(Token, null) = .empty;
    return tokens_list;
}

fn initKeywords(allocator: std.mem.Allocator) !std.StringHashMap(TokenType) {
    var keywords = std.StringHashMap(TokenType).init(allocator);

    try keywords.put("use", .USE);
    try keywords.put("let", .LET);
    try keywords.put("mut", .MUT);
    try keywords.put("const", .CONST);
    try keywords.put("global", .GLOBAL);
    try keywords.put("fn", .FN);
    try keywords.put("for", .FOR);
    try keywords.put("while", .WHILE);
    try keywords.put("return", .RETURN);
    //try keywords.put("struct", .STRUCT);
    //try keywords.put("enum", .ENUM);
    //try keywords.put("union", .UNION);
    try keywords.put("continue", .CONTINUE);
    try keywords.put("BREAK", .BREAK);
    try keywords.put("match", .MATCH);
    try keywords.put("if", .IF);
    try keywords.put("else", .ELSE);
    try keywords.put("std", .STD);
    try keywords.put("lib", .LIB);
    try keywords.put("extern", .EXTERN);
    try keywords.put("catch", .CATCH);
    try keywords.put("assert", .ASSERT);
    try keywords.put("suppress", .SUPPRESS);
    try keywords.put("exclude", .EXCLUDE);
    try keywords.put("throw", .THROW);
    try keywords.put("in", .IN);
    try keywords.put("as", .AS);
    try keywords.put("try", .TRY);
    try keywords.put("pub", .PUB);
    try keywords.put("and", .AND);
    try keywords.put("or", .OR);

    // primitive types here as well
    try keywords.put("u8", .UINT8);
    try keywords.put("u16", .UINT16);
    try keywords.put("u32", .UINT32);
    try keywords.put("u64", .UINT64);
    try keywords.put("i8", .INT8);
    try keywords.put("i16", .INT16);
    try keywords.put("i32", .INT32);
    try keywords.put("i64", .INT64);
    try keywords.put("f32", .FLOAT32);
    try keywords.put("f64", .FLOAT64);
    try keywords.put("bool", .BOOL);
    try keywords.put("void", .VOID);
    try keywords.put("c_int", .C_INT);
    try keywords.put("c_float", .C_FLOAT);
    try keywords.put("c_double", .C_DOUBLE);
    try keywords.put("c_char", .C_CHAR);

    return keywords;
}

pub const Tokenizer = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: i32,
    keywords: std.StringHashMap(TokenType),
    tokens: std.array_list.Aligned(Token, null),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Tokenizer {
        return Tokenizer{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .keywords = try initKeywords(allocator),
            .tokens = initTokens(),
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.keywords.deinit();
    }

    pub fn getTokens(self: *Tokenizer, allocator: std.mem.Allocator) !std.array_list.Aligned(Token, null) {
        while (!isAtEnd(self)) {
            self.start = self.current;
            try getToken(self, allocator);
        }

        // no EOF token anymore
        return self.tokens;
    }

    fn addToken(self: *Tokenizer, token_type: TokenType, allocator: std.mem.Allocator) !void {
        const text = self.source[self.start..self.current];
        const token_value = try allocator.dupe(u8, text);
        
        try self.tokens.append(allocator, Token{ .lexeme = token_value, .token_type = token_type });
    }

    fn getToken(self: *Tokenizer, alloc: std.mem.Allocator) !void {
        const char = advanceGetChar(self);

        switch (char) {
            '(' => try addToken(self, .LEFT_PAREN, alloc),
            ')' => try addToken(self, .RIGHT_PAREN, alloc),
            '[' => try addToken(self, .LEFT_BRACKET, alloc),
            ']' => try addToken(self, .RIGHT_BRACKET, alloc),
            '{' => try addToken(self, .LEFT_BRACE, alloc),
            '}' => try addToken(self, .RIGHT_BRACE, alloc),
            ',' => try addToken(self, .COMMA, alloc),
            '.' => try addToken(self, .DOT, alloc),
            '-' => {
                if (match(self, '>')) {
                    try addToken(self, .RIGHT_ARROW, alloc);
                    return;
                }
                if (match(self, '=')) try addToken(self, .MINUS_EQUAL, alloc)
                else try addToken(self, .MINUS, alloc);
            },
            '+' => if (match(self, '=')) try addToken(self, .PLUS_EQUAL, alloc) else try addToken(self, .PLUS, alloc),
            ';' => try addToken(self, .SEMICOLON, alloc),
            '/' => {
                if (peek(self) == '*') {
                    while (peek(self) != '*' or peekNext(self) != '/') advance(self);

                    advance(self);
                    advance(self);
                } else if (match(self, '/')) {
                    while (peek(self) != '\n' and !isAtEnd(self)) advance(self);
                } else {
                    if (match(self, '=')) try addToken(self, .SLASH_EQUAL, alloc) else try addToken(self, .SLASH, alloc);
                }
            },
            '*' => if (match(self, '=')) try addToken(self, .STAR_EQUAL, alloc) else try addToken(self, .STAR, alloc),
            '?' => try addToken(self, .QUERY, alloc),
            ':' => try addToken(self, .COLON, alloc),

            '!' => if (match(self, '=')) try addToken(self, .BANG_EQUAL, alloc) else try addToken(self, .BANG, alloc),
            '=' => if (match(self, '=')) try addToken(self, .EQUAL_EQUAL, alloc) else try addToken(self, .EQUAL, alloc),
            '>' => if (match(self, '=')) try addToken(self, .GREATER_EQUAL, alloc) else try addToken(self, .GREATER, alloc),
            '<' => if (match(self, '=')) try addToken(self, .LESS_EQUAL, alloc) else try addToken(self, .LESS, alloc),

            ' ', '\t', '\r' => {},

            '\n' => self.line += 1,

            '"' => try string(self, alloc),
            '\'' => try character(self, alloc),

            else => {
                if (isDigit(char)) {
                    try number(self, alloc);
                } else if (isAlphaNumeric(char)) {
                    try identifier(self, alloc);
                } else {
                    main.reportError(self.line, "", "Unexpected character");
                }
            },
        }
    }

    fn character(self: *Tokenizer, alloc: std.mem.Allocator) !void {
        if (peekNext(self) != '\'') return;

        advance(self);
        advance(self);

        try addToken(self, .CHARACTER, alloc);
    }

    fn string(self: *Tokenizer, alloc: std.mem.Allocator) !void {
        while (peek(self) != '"' and !isAtEnd(self)) {
            if (peek(self) == '\n') self.line += 1;
            advance(self);
        }

        advance(self);

        try addToken(self, .STRING, alloc);
    }

    fn peek(self: *Tokenizer) u8 {
        if (isAtEnd(self)) return 0;
        return self.source[self.current];
    }

    fn isAtEnd(self: *Tokenizer) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Tokenizer) void {
        self.current += 1;
    }

    fn advanceGetChar(self: *Tokenizer) u8 {
        const char = self.source[self.current];
        self.current += 1;
        return char;
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn number(self: *Tokenizer, alloc: std.mem.Allocator) !void {
        while (isDigit(peek(self))) advance(self);

        if (peek(self) == '.' and isDigit(peekNext(self))) {
            advance(self);

            while (isDigit(peek(self))) advance(self);

            try addToken(self, .FLOAT, alloc);
            return;
        }

        try addToken(self, .INTEGER, alloc);
    }

    fn peekNext(self: *Tokenizer) u8 {
        return self.source[self.current + 1];
    }

    fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or
            (char >= 'A' and char <= 'Z') or
            (char == '_');
    }

    fn identifier(self: *Tokenizer, alloc: std.mem.Allocator) !void {
        while (isAlphaNumeric(peek(self))) advance(self);

        const text = self.source[self.start..self.current];
        const token_type = self.keywords.get(text) orelse TokenType.IDENTIFIER;

        try addToken(self, token_type, alloc);
    }

    fn isAlphaNumeric(char: u8) bool {
        return isAlpha(char) or isDigit(char);
    }

    fn match(self: *Tokenizer, char: u8) bool {
        if (isAtEnd(self)) return false;
        if (self.source[self.current] != char) return false;

        advance(self);
        return true;
    }
};
