const std = @import("std");

const TokenType = enum {
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
    QUERY, COLON,

    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

    PLUS_EQUAL, MINUS_EQUAL, TIMES_EQUAL, DIVIDE_EQUAL,
    PLUS_PLUS, MINUS_MINUS,

    UINT8, UINT16, UINT32, UINT64, INT8, INT16, INT32,
    INT64, FLOAT32, FLOAT64, BOOL, VOID,

    USE, LET, MUT, CONST, GLOBAL, FN, FOR, WHILE, RETURN,
    STRUCT, ENUM, UNION, CONTINUE, BREAK, MATCH, IF, ELSE,
    STD, LIB, EXTERN, CATCH, ASSERT, SUPPRESS, EXCLUDE, THROW,
    IN, AS, TRY,

    EOF,
};

const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    literal: anytype,
    line: i32,
};

const Keyword = struct {
    token_type: TokenType,
    name: []const u8,
}

fn keyword(token_type: TokenType, name: []const u8) Keyword {
    return Keyword{
        .token_type = token_type,
        .name = name,
    }
}

pub const keywords = [_]Keyword{
    keyword(.USE = "use"),
    keyword(.LET = "let"),
    keyword(.MUT = "mut"),
    keyword(.CONST = "const"),
    keyword(.GLOBAL = "global"),
    keyword(.FN = "fn"),
    keyword(.FOR = "for"),
    keyword(.WHILE = "while"),
    keyword(.RETURN = "return"),
    keyword(.STRUCT = "struct"),
    keyword(.ENUM = "enum"),
    keyword(.UNION = "union"),
    keyword(.CONTINUE = "continue"),
    keyword(.BREAK = "break"),
    keyword(.MATCH = "match"),
    keyword(.IF = "if"),
    keyword(.ELSE = "else"),
    keyword(.STD = "std"),
    keyword(.LIB = "lib"),
    keyword(.EXTERN = "extern"),
    keyword(.CATCH = "catch"),
    keyword(.ASSERT = "assert"),
    keyword(.SUPPRESS = "suppress"),
    keyword(.EXCLUDE = "exclude"),
    keyword(.THROW = "throw"),
    keyword(.IN = "in"),
    keyword(.AS = "as"),
    keyword(.TRY = "try"),
};

const Scanner = struct {
    source: []u8,
    start: i32,
    current: i32,
    line: i32,

    pub fn init(source: []u8) Scanner {
        return Scanner {
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }
};
