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

pub const TokenError = struct {
    message: []const u8,
    line: usize,
    col: usize,
};

fn initErrors() std.ArrayList(TokenError) {
    const errs: std.array_list.Aligned(TokenError, null) = .empty;
    return errs;
}

pub const Tokenizer = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    col: usize,
    keywords: std.StaticStringMap(TokenType),
    tokens: std.ArrayList(Token),
    alloc: std.mem.Allocator,
    errors: std.ArrayList(TokenError),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Tokenizer {
        return Tokenizer{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .col = 1,
            .keywords = try initKeywords(),
            .tokens = initTokens(),
            .errors = initErrors(),
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
                .start_col = self.start,
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
                    const start_line = self.line;
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

                    if (isAtEnd(self)) {
                        const msg = try std.fmt.allocPrint(
                            self.alloc,
                            "Unterminated block comment starting at line {d}",
                            .{start_line}
                        );
                        try collectError(self, msg);
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
                    const msg = try std.fmt.allocPrint(
                        self.alloc,
                        "Unexpected character: '{c}'",
                        .{char}
                    );
                    try collectError(self, msg);
                }
            },
        }
    }

    fn character(self: *Tokenizer) !void {
        const start_line = self.line;
        const start_col = self.col - 1; // We already consumed opening '
    
        var char_count: usize = 0;
        var has_escape = false;
    
        while (peek(self) != '\'' and !isAtEnd(self)) {
            // check if we hit an actual newline (not escaped \n)
            if (peek(self) == '\n') {
                const msg = try std.fmt.allocPrint(
                    self.alloc,
                    "Unterminated character literal at line {d}, col {d} (cannot span multiple lines)",
                    .{start_line, start_col}
                );
                try collectError(self, msg);
                return; // don't consume the newline
            }
        
            // handle escape sequences
            if (peek(self) == '\\') {
                has_escape = true;
                advance(self); // consume backslash
            
                if (isAtEnd(self)) {
                    try collectError(self, "Unterminated character literal: unexpected end of file");
                    return;
                }
            
                const escape_char = peek(self);
                switch (escape_char) {
                    'n', 't', 'r', '\\', '\'', '"', '0' => {
                        advance(self); // consume escape character
                        char_count += 1; // this counts as one character
                    },
                    'x' => {
                        // hex escape
                        advance(self); // consume 'x'
                    
                        // expect 2 hex digits
                        var hex_digits: usize = 0;
                        while (hex_digits < 2 and isHexDigit(peek(self))) {
                            advance(self);
                            hex_digits += 1;
                        }
                    
                        if (hex_digits != 2) {
                            const msg = try std.fmt.allocPrint(
                                self.alloc,
                                "Invalid hex escape sequence: expected 2 hex digits at line {d}, col {d}",
                                .{self.line, self.col}
                            );
                            try collectError(self, msg);
                        }
                        char_count += 1;
                    },
                    'u' => {
                        // Unicode escape like '\u0041'
                        advance(self); // consume 'u'
                    
                        var hex_digits: usize = 0;
                        while (hex_digits < 4 and isHexDigit(peek(self))) {
                            advance(self);
                            hex_digits += 1;
                        }
                    
                        if (hex_digits != 4) {
                            const msg = try std.fmt.allocPrint(
                                self.alloc,
                                "Invalid unicode escape sequence: expected 4 hex digits at line {d}, col {d}",
                                .{self.line, self.col}
                            );
                            try collectError(self, msg);
                        }
                        char_count += 1;
                    },
                    else => {
                        const msg = try std.fmt.allocPrint(
                            self.alloc,
                            "Invalid escape sequence '\\{c}' at line {d}, col {d}",
                            .{escape_char, self.line, self.col}
                        );
                        try collectError(self, msg);
                        advance(self);
                        char_count += 1;
                    },
                }
            } else {
                // Regular character
                advance(self);
                char_count += 1;
            }
        }
    
        if (isAtEnd(self)) {
            const msg = try std.fmt.allocPrint(
                self.alloc,
                "Unterminated character literal starting at line {d}, col {d}",
                .{start_line, start_col}
            );
            try collectError(self, msg);
            return;
        }
    
        // check for empty character literal
        if (char_count == 0) {
            const msg = try std.fmt.allocPrint(
                self.alloc,
                "Empty character literal at line {d}, col {d}",
                .{start_line, start_col}
            );
            try collectError(self, msg);
        }
    
        // check for multiple characters
        if (char_count > 1) {
            const msg = try std.fmt.allocPrint(
                self.alloc,
                "Character literal contains multiple characters at line {d}, col {d} (use string literal instead)",
                .{start_line, start_col}
            );
            try collectError(self, msg);
        }
    
        advance(self);
        try addToken(self, .CHARACTER);
    }

    fn isHexDigit(char: u8) bool {
        return (char >= '0' and char <= '9') or
               (char >= 'a' and char <= 'f') or
               (char >= 'A' and char <= 'F');
    }

    fn string(self: *Tokenizer) !void {
        const start_line = self.line;
        const start_col = self.col;
        
        while (peek(self) != '"' and !isAtEnd(self)) {
            if (peek(self) == '\n') {
                self.line += 1;
                resetCol(self);
            }
            advance(self);
        }

        if (isAtEnd(self)) {
            const msg = try std.fmt.allocPrint(
                self.alloc,
                "Unterminated string at line {d} col {d}",
                .{start_line, start_col}
            );
            try collectError(self, msg);
            try addToken(self, .STRING);
            return;
        }

        advance(self);
        try addToken(self, .STRING);
    }

    fn expect(self: *Tokenizer, char: u8) !void {
        if (self.source[self.current] != char) {
            return error.UnexpectedCharacter;
        }
        
        self.current += 1;
        self.col += 1;
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
        const start_col = self.col - 1;
        var has_error = false;
    
        while (isDigit(peek(self)) or peek(self) == '_') {
            advance(self);
        }
    
        var is_float = false;
        var decimal_count: usize = 0;
    
        while (peek(self) == '.') {
            const has_digit_after = hasDigitAfterDot(self);
        
            if (has_digit_after) {
                decimal_count += 1;
                is_float = true;
                advance(self);
            
                if (decimal_count > 1) {
                    has_error = true;
                    const msg = try std.fmt.allocPrint(
                        self.alloc,
                        "Invalid number: multiple decimal points at line {d}, col {d}",
                        .{self.line, self.col - 1}
                    );
                    try collectError(self, msg);
                }
            
                if (peek(self) == '_') {
                    has_error = true;
                    const msg = try std.fmt.allocPrint(
                        self.alloc,
                        "Invalid number: underscore cannot immediately follow decimal point at line {d}, col {d}",
                        .{self.line, self.col}
                    );
                    try collectError(self, msg);
                }
            
                while (isDigit(peek(self)) or peek(self) == '_') {
                    advance(self);
                }
            } else {
                break;
            }
        }
    
        const lexeme = self.source[self.start..self.current];
    
        if (lexeme[lexeme.len - 1] == '_') {
            has_error = true;
            const msg = try std.fmt.allocPrint(
                self.alloc,
                "Invalid number: trailing underscore at line {d}, col {d}",
                .{self.line, self.col - 1}
            );
            try collectError(self, msg);
        }
    
        var i: usize = 0;
        while (i < lexeme.len) : (i += 1) {
            if (lexeme[i] == '_' and i + 1 < lexeme.len and lexeme[i + 1] == '.') {
                has_error = true;
                const msg = try std.fmt.allocPrint(
                    self.alloc,
                    "Invalid number: underscore before decimal point at line {d}, col {d}",
                    .{self.line, start_col + i}
                );
                try collectError(self, msg);
            }
        }
    
        var prev_was_underscore = false;
        for (lexeme) |char| {
            if (char == '_') {
                if (prev_was_underscore) {
                    has_error = true;
                    const msg = try std.fmt.allocPrint(
                        self.alloc,
                        "Invalid number: consecutive underscores at line {d}, col {d}",
                        .{self.line, start_col}
                    );
                    try collectError(self, msg);
                    break;
                }
                prev_was_underscore = true;
            } else {
                prev_was_underscore = false;
            }
        }
    
        if (isAlpha(peek(self))) {
            has_error = true;
            const suffix_start = self.current;
            while (isAlphaNumeric(peek(self))) advance(self);
        
            const invalid_suffix = self.source[suffix_start..self.current];
            const msg = try std.fmt.allocPrint(
                self.alloc,
                "Invalid number: unexpected suffix '{s}' at line {d}, col {d}",
                .{invalid_suffix, self.line, start_col}
            );
            try collectError(self, msg);
        }
    
        if (is_float) {
            try addToken(self, .FLOAT);
        } else {
            try addToken(self, .INTEGER);
        }
    }

    fn hasDigitAfterDot(self: *Tokenizer) bool {
        var lookahead: usize = 1;
    
        while (self.current + lookahead < self.source.len) {
            const char = self.source[self.current + lookahead];
        
            if (isDigit(char)) {
                return true;
            }

            if (char == '_') {
                lookahead += 1;
                continue;
            }

            return false;
        }
    
        return false;
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

    fn collectError(self: *Tokenizer, message: []const u8) !void {
        try self.errors.append(self.alloc, TokenError{
            .message = message,
            .line = self.line,
            .col = self.col,
        });
    }
};

// ====================================
// TESTS
// ====================================

const testing = std.testing;

fn testTokenize(allocator: std.mem.Allocator, source: []const u8) ![]Token {   
    var tokenizer = try Tokenizer.init(allocator, source);
    return try tokenizer.getTokens();
}

fn expectTokens(allocator: std.mem.Allocator, source: []const u8, expected: []const Token) !void {
    const output = try testTokenize(allocator, source);

    for (expected, output) |exp_token, actual| {
        try testing.expectEqual(exp_token.token_type, actual.token_type);
        try testing.expectEqualStrings(exp_token.lexeme, actual.lexeme);
    }
}

fn expectError(source: []const u8, expected_error_count: usize) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var tokenizer = try Tokenizer.init(allocator, source);
    _ = try tokenizer.getTokens();

    try testing.expectEqual(expected_error_count, tokenizer.errors.items.len);
}

fn expectTokensWithErrors(
    source: []const u8,
    expected: []const Token,
    expected_error_count: usize,
) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var tokenizer = try Tokenizer.init(allocator, source);
    const output = try tokenizer.getTokens();

    try testing.expectEqual(expected_error_count, tokenizer.errors.items.len);

    for (expected, output) |exp_token, actual| {
        try testing.expectEqual(exp_token.token_type, actual.token_type);
        try testing.expectEqualStrings(exp_token.lexeme, actual.lexeme);
    }
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

test "valid character literals" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
       
    try expectTokens(allocator, "'a'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "a", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
    
    try expectTokens(allocator, "'\\n'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\n", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
    
    try expectTokens(allocator, "'\\t'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\t", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
    
    try expectTokens(allocator, "'\\\\'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\\\", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
    
    try expectTokens(allocator, "'\\''", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\'", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "character literal with hex escape" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "'\\x41'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\x41", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "character literal with unicode escape" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "'\\u0041'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\u0041", .span = null },
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

test "string with newline" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    try expectTokens(allocator, "\"hello\nworld\"", &.{
        .{ .token_type = .STRING, .lexeme = "hello\nworld", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "empty string" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    try expectTokens(allocator, "\"\"", &.{
        .{ .token_type = .STRING, .lexeme = "", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "operators without spaces" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "x+*y", &.{
        .{ .token_type = .IDENTIFIER, .lexeme = "x", .span = null },
        .{ .token_type = .PLUS, .lexeme = "+", .span = null },
        .{ .token_type = .STAR, .lexeme = "*", .span = null },
        .{ .token_type = .IDENTIFIER, .lexeme = "y", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "numbers with underscores" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    try expectTokens(allocator, "1_000_000", &.{
        .{ .token_type = .INTEGER, .lexeme = "1_000_000", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });

    try expectTokens(allocator, "3_14.15_92", &.{
        .{ .token_type = .FLOAT, .lexeme = "3_14.15_92", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "number just at end of file" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "123", &.{
        .{ .token_type = .INTEGER, .lexeme = "123", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "valid floating point number" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    try expectTokens(allocator, "3.14", &.{
        .{ .token_type = .FLOAT, .lexeme = "3.14", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "integer followed by dot and identifier" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "123.abc", &.{
        .{ .token_type = .INTEGER, .lexeme = "123", .span = null },
        .{ .token_type = .DOT, .lexeme = ".", .span = null },
        .{ .token_type = .IDENTIFIER, .lexeme = "abc", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "character literal followed by other tokens" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "'a' + 'b'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "a", .span = null },
        .{ .token_type = .PLUS, .lexeme = "+", .span = null },
        .{ .token_type = .CHARACTER, .lexeme = "b", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

test "underscore followed by number" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    const allocator = arena.allocator();
    defer arena.deinit();
    
    try expectTokens(allocator, "_123", &.{
        .{ .token_type = .IDENTIFIER, .lexeme = "_123", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    });
}

// ====================================
// ERROR CASES
// ====================================

// TODO test incorrect escape sequences

// '@' may be added later for built-in functions
test "invalid character" {
    try expectError("@", 1);
}

test "unterminated string" {
    try expectError("\"hello", 1);
}

test "unterminated character" {
    try expectError("'a", 1);
}

test "unterminated block comment" {
    try expectError("/* invalid", 1);
}

test "invalid character followed by valid tokens" {
    try expectTokensWithErrors("@@@ x = 5", &.{
        .{ .token_type = .IDENTIFIER, .lexeme = "x", .span = null },
        .{ .token_type = .EQUAL, .lexeme = "=", .span = null },
        .{ .token_type = .INTEGER, .lexeme = "5", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 3);
}

test "multiple sequential errors" {
    try expectError("@@@ ### $$$", 9);
}

test "number followed by identifier" {
    try expectError("163zsd", 1);
}

test "multiple decimal points in number" {
    try expectTokensWithErrors("1.234.567", &.{
        .{ .token_type = .FLOAT, .lexeme = "1.234.567", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}

test "invalid underscore patterns" {
    try expectTokensWithErrors("123_", &.{
        .{ .token_type = .INTEGER, .lexeme = "123_", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
    
    try expectTokensWithErrors("1__000", &.{
        .{ .token_type = .INTEGER, .lexeme = "1__000", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
    
    try expectTokensWithErrors("123_.45", &.{
        .{ .token_type = .FLOAT, .lexeme = "123_.45", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
    
    try expectTokensWithErrors("123._45", &.{
        .{ .token_type = .FLOAT, .lexeme = "123._45", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}

test "number with invalid suffix" {
    try expectTokensWithErrors("123abc", &.{
        .{ .token_type = .INTEGER, .lexeme = "123abc", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}

test "float with invalid suffix" {
    try expectTokensWithErrors("3.14xyz", &.{
        .{ .token_type = .FLOAT, .lexeme = "3.14xyz", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}

test "invalid character literals - multiple characters" {
    try expectTokensWithErrors("'abc'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "abc", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}

test "invalid character literals - empty" {
    try expectTokensWithErrors("''", &.{
        .{ .token_type = .CHARACTER, .lexeme = "", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}

test "invalid escape sequences in character literals" {
    try expectTokensWithErrors("'\\q'", &.{
        .{ .token_type = .CHARACTER, .lexeme = "\\q", .span = null },
        .{ .token_type = .EOF, .lexeme = "", .span = null },
    }, 1);
}
