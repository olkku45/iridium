const std = @import("std");
const main = @import("main.zig");
const print = std.debug.print;

const Span = @import("main.zig").Span;

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
    MODULUS,
    
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
    TRUE,
    FALSE,
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
    NULL,
    
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
    SWITCH,
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

    EOF,
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    span: ?Span,
};

fn initTokens() std.array_list.Aligned(Token, null) {
    const tokens_list: std.array_list.Aligned(Token, null) = .empty;
    return tokens_list;
}

fn initKeywords() !std.StaticStringMap(TokenType) {
    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        .{"use", .USE},
        .{"let", .LET},
        .{"mut", .MUT},
        .{"const", .CONST},
        .{"global", .GLOBAL},
        .{"fn", .FN},
        .{"for", .FOR},
        .{"while", .WHILE},
        .{"return", .RETURN},
        .{"continue", .CONTINUE},
        .{"break", .BREAK},
        .{"switch", .SWITCH},
        .{"if", .IF},
        .{"else", .ELSE},
        .{"std", .STD},
        .{"lib", .LIB},
        .{"extern", .EXTERN},
        .{"catch", .CATCH},
        .{"assert", .ASSERT},
        .{"suppress", .SUPPRESS},
        .{"throw", .THROW},
        .{"in", .IN},
        .{"as", .AS},
        .{"try", .TRY},
        .{"pub", .PUB},
        .{"and", .AND},
        .{"or", .OR},
        .{"u8", .UINT8},
        .{"u16", .UINT16},
        .{"u32", .UINT32},
        .{"u64", .UINT64},
        .{"i8", .INT8},
        .{"i16", .INT16},
        .{"i32", .INT32},
        .{"i64", .INT64},
        .{"f32", .FLOAT32},
        .{"f64", .FLOAT64},
        .{"bool", .BOOL},
        .{"true", .TRUE},
        .{"false", .FALSE},
        .{"void", .VOID},
        .{"c_int", .C_INT},
        .{"c_float", .C_FLOAT},
        .{"c_double", .C_DOUBLE},
        .{"c_char", .C_CHAR},
        .{"null", .NULL},
    });

    return keywords;
}

const Error = error{
    SemicolonNotAtEOL,
    WrongCharacter,
    UnexpectedCharacter,
};

pub const Tokenizer = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    col: usize,
    keywords: std.StaticStringMap(TokenType),
    tokens: std.ArrayList(Token),
    alloc: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Tokenizer {
        return Tokenizer{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .col = 1,
            .keywords = try initKeywords(),
            .tokens = initTokens(),
            .alloc = allocator,
        };
    }

    pub fn getTokens(self: *Tokenizer) ![]Token {
        while (!isAtEnd(self)) {
            self.start = self.current;
            try getToken(self);
        }

        try self.tokens.append(self.alloc, Token{
            .token_type = .EOF,
            .lexeme = "",
            .span = Span{
                .line = self.line,
                .start_col = self.col,
                .end_col = self.col,
                .source_file = null,
            }
        });

        return self.tokens.toOwnedSlice(self.alloc);
    }

    // TODO can we remove the allocating from this? this was causing some bug
    // without the heap allocation
    fn addToken(self: *Tokenizer, token_type: TokenType) !void {
        var text = self.source[self.start..self.current];

        // remove quotes
        if (text[0] == '"' and text[text.len - 1] == '"'
        or text[0] == '\'' and text[text.len - 1] == '\'') {
            text = text[1..text.len - 1];
        }
                
        const token_value = try self.alloc.dupe(u8, text);
        
        try self.tokens.append(self.alloc, Token{
            .lexeme = token_value,
            .token_type = token_type,
            .span = Span{
                .start_col = self.col - text.len,
                .end_col = self.col,
                .line = self.line,
                .source_file = null,
            }
         });
    }

    fn getToken(self: *Tokenizer) !void {
        const char = getCharAdvance(self);

        switch (char) {
            '(' => try addToken(self, .LEFT_PAREN),
            ')' => try addToken(self, .RIGHT_PAREN),
            '[' => try addToken(self, .LEFT_BRACKET),
            ']' => try addToken(self, .RIGHT_BRACKET),
            '{' => try addToken(self, .LEFT_BRACE),
            '}' => try addToken(self, .RIGHT_BRACE),
            ',' => try addToken(self, .COMMA),
            '.' => try addToken(self, .DOT),
            '-' => {
                if (match(self, '=')) try addToken(self, .MINUS_EQUAL)
                else try addToken(self, .MINUS);
            },
            '+' => if (match(self, '=')) try addToken(self, .PLUS_EQUAL) else try addToken(self, .PLUS),
            ';' => try addToken(self, .SEMICOLON),
            '/' => {
                if (peek(self) == '*') {
                    advance(self);
                    while (!isAtEnd(self)) {
                        if (peek(self) == '*' and peekNext(self) == '/') {
                            advance(self);
                            advance(self);
                            break;
                        }
                        if (peek(self) == '\n') {
                            self.line += 1;
                            resetCol(self);
                        }
                        advance(self);
                    }
                } else if (match(self, '/')) {
                    while (peek(self) != '\n' and !isAtEnd(self)) advance(self);
                } else {
                    if (match(self, '=')) try addToken(self, .SLASH_EQUAL) else try addToken(self, .SLASH);
                }
            },
            '*' => if (match(self, '=')) try addToken(self, .STAR_EQUAL) else try addToken(self, .STAR),
            '?' => try addToken(self, .QUERY),
            ':' => try addToken(self, .COLON),
            '%' => try addToken(self, .MODULUS),

            '!' => if (match(self, '=')) try addToken(self, .BANG_EQUAL) else try addToken(self, .BANG),
            '=' => {
                if (match(self, '>')) {
                    try addToken(self, .RIGHT_ARROW);
                    return;
                }
                if (match(self, '=')) try addToken(self, .EQUAL_EQUAL)
                else try addToken(self, .EQUAL);
            },
            '>' => if (match(self, '=')) try addToken(self, .GREATER_EQUAL) else try addToken(self, .GREATER),
            '<' => if (match(self, '=')) try addToken(self, .LESS_EQUAL) else try addToken(self, .LESS),

            ' ', '\t', '\r' => {},

            '\n' => {
                self.line += 1;
                resetCol(self);
            },

            '"' => try string(self),
            '\'' => try character(self),

            else => {
                if (isDigit(char)) {
                    try number(self);
                } else if (isAlphaNumeric(char)) {
                    try identifier(self);
                } else {
                    return error.UnexpectedCharacter;
                }
            },
        }
    }

    // 'a', '\n', '\''
    fn character(self: *Tokenizer) !void {
        if (peek(self) == '\\') {
            while (!isAtEnd(self) and peek(self) != '\'') advance(self);
            if (!isAtEnd(self)) advance(self);
        } else {
            if (!isAtEnd(self)) advance(self);
        }

        try addToken(self, .CHARACTER);
    }

    fn string(self: *Tokenizer) !void {
        while (peek(self) != '"' and !isAtEnd(self)) {
            if (peek(self) == '\n') {
                self.line += 1;
                resetCol(self);
            }
            advance(self);
        }

        advance(self);

        try addToken(self, .STRING);
    }

    fn peek(self: *Tokenizer) u8 {
        if (isAtEnd(self)) return 0;
        return self.source[self.current];
    }

    fn isAtEnd(self: *Tokenizer) bool {
        return self.current >= self.source.len;
    }

    fn isAtStart(self: *Tokenizer) bool {
        return self.current == 0;
    }

    fn advance(self: *Tokenizer) void {
        self.current += 1;
        self.col += 1;
    }

    fn resetCol(self: *Tokenizer) void {
        self.col = 1;
    }

    fn getCharAdvance(self: *Tokenizer) u8 {
        const char = self.source[self.current];
        self.current += 1;
        self.col += 1;
        return char;
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn number(self: *Tokenizer) !void {
        while (isDigit(peek(self))) advance(self);

        if (peek(self) == '.' and isDigit(peekNext(self))) {
            advance(self);

            while (isDigit(peek(self))) advance(self);

            try addToken(self, .FLOAT);
            return;
        }

        try addToken(self, .INTEGER);
    }

    fn peekNext(self: *Tokenizer) u8 {
        return self.source[self.current + 1];
    }

    fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or
            (char >= 'A' and char <= 'Z') or
            (char == '_');
    }

    fn identifier(self: *Tokenizer) !void {
        while (isAlphaNumeric(peek(self))) advance(self);

        const text = self.source[self.start..self.current];
        const token_type = self.keywords.get(text) orelse TokenType.IDENTIFIER;

        try addToken(self, token_type);
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

// ====================================
// TESTS
// ====================================

const testing = std.testing;

fn expectTokens(allocator: std.mem.Allocator, source: []const u8, expected: []const Token) !void {
    const output = testTokenize(allocator, source);

    for (expected, output) |exp_token, actual| {
        try testing.expectEqual(exp_token.token_type, actual.token_type);
        try testing.expectEqualStrings(exp_token.lexeme, actual.lexeme);
    }
}

fn testTokenize(allocator: std.mem.Allocator, source: []const u8) ![]Token {   
    var tokenizer = try Tokenizer.init(allocator, source);
    return try tokenizer.getTokens();
}

test "basic tests" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
      
    try expectTokens(allocator, "(", &.{
        .{ .token_type = .LEFT_PAREN, .lexeme = "(", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });

    try expectTokens(allocator, "+", &.{
        .{ .token_type = .PLUS, .lexeme = "+", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });

    try expectTokens(allocator, "42 + 1", &.{
        .{ .token_type = .INTEGER, .lexeme = "42", .span = null },
        .{ .token_type = .PLUS, .lexeme = "+", .span = null },
        .{ .token_type = .INTEGER, .lexeme = "1", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "identifiers" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "x + y", &.{
        .{ .token_type = .IDENTIFIER, .lexeme = "x", .span = null },
        .{ .token_type = .PLUS, .lexeme = "+", .span = null },
        .{ .token_type = .IDENTIFIER, .lexeme = "y", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "character literals" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
      
    try expectTokens(allocator, "'a' '\n'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "a", .span = null },
        .{ .token_type = .CHARACTER, .lexeme = "\n", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null }, 
    });
}

test "string literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
      
    try expectTokens(allocator, "\"hello world\"", &.{
        .{ .token_type = .STRING, .lexeme = "hello world", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "keywords" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
     
    try expectTokens(allocator, "for std fn extern", &.{
        .{ .token_type = .FOR, .lexeme = "for", .span = null },
        .{ .token_type = .STD, .lexeme = "std", .span = null },
        .{ .token_type = .FN, .lexeme = "fn", .span = null },
        .{ .token_type = .EXTERN, .lexeme = "extern", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

// ====================================
// ERROR CASES
// ====================================

// '@' may be added later for built-in functions
test "invalid character" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    const err = testTokenize(allocator, "@");
    try testing.expectError(error.UnexpectedCharacter, err);
}

test "multiple characters in char literal" {
    
}
