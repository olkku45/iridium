// README:
// this parser is only intended to handle a 'putchar()' call
// also the debug prints are for printing the identifier name and
// argument literal since the AST print doesn't want to print those out

const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const main = @import("main.zig");

const Token = Tokenizer.Token;
const TokenType = Tokenizer.TokenType;

const Error = error{
    IndexError,
    NotPutChar,
};

const Nodes = struct {
    fn ProgramNode() type {
        return struct {
            program: FunctionCallNode(),
        };
    }

    fn IdentifierNode() type {
        return struct {
            name: []const u8
        };
    }

    fn ArgumentNode() type {
        return struct {
            argument_literal: LiteralNode(),  
        };
    }

    fn LiteralNode() type {
        return struct {
            value: []const u8,
        };
    }

    fn FunctionCallNode() type {
        return struct {
            identifier: IdentifierNode(),
            call_argument: ArgumentNode(),
        };
    }
};

pub const Parser = struct {
    tokens: std.array_list.Aligned(Token, null),
    current: usize,

    pub fn init(tokens_list: std.array_list.Aligned(Token, null)) Parser {
        return Parser{
            .tokens = tokens_list,
            .current = 0,        
        };
    }

    // parse putchar()
    pub fn parseTokens(self: *Parser) !Nodes.ProgramNode() {
        const function_call = try functionCall(self);
        return Nodes.ProgramNode() {.program = function_call};
    }
    
    // no need yet at least
    fn advanceGetCurrentToken(self: *Parser) Token {
        const current_token = self.tokens.items[self.current];
        self.current += 1;
        return current_token;
    }

    // no need yet at least
    fn previous(self: *Parser) !Token {
        if (self.current > 0) return self.tokens.items[self.current - 1]
        else return Error.IndexError;
    }

    // no need yet at least
    fn next(self: *Parser) !Token {
        if (self.current + 1 <= self.tokens.items.len)
        return self.tokens.items[self.current + 1]
        else return Error.IndexError;
    }
    
    // FunctionCall : Identifier "(" Argument ")"
    fn functionCall(self: *Parser) !Nodes.FunctionCallNode() {
        const ident = identifier(self);
        // 'current' because calling identifier() advances current token
        if (self.tokens.items[self.current].token_type != .LEFT_PAREN) return Error.NotPutChar;

        const arg = argument(self);

        return Nodes.FunctionCallNode() {.call_argument = arg, .identifier = ident};        
    }

    // Identifier : \String of characters not reserved as a token\
    fn identifier(self: *Parser) Nodes.IdentifierNode() {
        const name = self.tokens.items[self.current].lexeme;
        // README: print identifier
        std.debug.print("identifier: {s}\n", .{name});
        const ident = Nodes.IdentifierNode() {.name = name};
        
        advance(self);

        return ident;
    }

    // Argument : Literal
    fn argument(self: *Parser) Nodes.ArgumentNode() {
        const lit = literal(self);
        // README: print argument characters
        std.debug.print("argument: {any}\n", .{lit});
        const arg = Nodes.ArgumentNode() {.argument_literal = lit};
        
        advance(self);
        return arg;
    }

    // Literal : \Any direct source code value\
    fn literal(self: *Parser) Nodes.LiteralNode() {
        // self.current + 1 to take the character from putchar call
        const lexeme = self.tokens.items[self.current + 1].lexeme;
        return Nodes.LiteralNode() {.value = lexeme};
    }

    fn advance(self: *Parser) void {
        self.current += 1;
    }
};
