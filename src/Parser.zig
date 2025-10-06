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

pub const Node = union(enum) {
    program: ProgramNode,
    identifier: IdentifierNode,
    argument: ArgumentNode,
    literal: LiteralNode,
    function_call: FunctionCallNode,

    pub const ProgramNode = struct {
        program_ast: *Node,
    };

    pub const IdentifierNode = struct {
        name: []const u8,
    };

    pub const ArgumentNode = struct {
        argument_literal: *Node,
    };

    pub const LiteralNode = struct {
        value: []const u8,
    };

    pub const FunctionCallNode = struct {
        identifier: *Node,
        call_argument: *Node,
    };
};

pub const Parser = struct {
    tokens: std.array_list.Aligned(Token, null),
    current: usize,
    allocator: std.mem.Allocator,

    pub fn init(tokens_list: std.array_list.Aligned(Token, null), allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens_list,
            .current = 0,
            .allocator = allocator,        
        };
    }

    // parse putchar()
    pub fn parseTokens(self: *Parser) !Node {
        const function_call = try functionCall(self);
        const call_ptr = try self.allocator.create(Node);
        call_ptr.* = function_call;

        return Node {.program = .{
            .program_ast = call_ptr,
        }};
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
    fn functionCall(self: *Parser) !Node {
        const ident = identifier(self);
        const ident_ptr = try self.allocator.create(Node);
        ident_ptr.* = ident;

        // 'current' because calling identifier() advances current token
        if (self.tokens.items[self.current].token_type != .LEFT_PAREN) return Error.NotPutChar;

        const arg = try argument(self);
        const arg_ptr = try self.allocator.create(Node);
        arg_ptr.* = arg;

        return Node {.function_call = .{
            .call_argument = arg_ptr,
            .identifier = ident_ptr,
        }};        
    }

    // Identifier : \String of characters not reserved as a token\
    fn identifier(self: *Parser) Node {
        const name = self.tokens.items[self.current].lexeme;

        // README: print identifier
        //std.debug.print("identifier: {s}\n", .{name});
        const ident = Node {.identifier = .{
            .name = name,
        }};
        
        advance(self);

        return ident;
    }

    // Argument : Literal
    fn argument(self: *Parser) !Node {
        const lit = literal(self);
        const lit_ptr = try self.allocator.create(Node);
        lit_ptr.* = lit;
        
        // README: print argument characters
        //std.debug.print("argument: {any}\n", .{lit});
        const arg = Node {.argument = .{
            .argument_literal = lit_ptr,
        }};
        
        advance(self);
        return arg;
    }

    // Literal : \Any direct source code value\
    fn literal(self: *Parser) Node {
        // self.current + 1 to take the character from putchar call
        const lexeme = self.tokens.items[self.current + 1].lexeme;
        return Node {.literal = .{
            .value = lexeme,
        }};
    }

    fn advance(self: *Parser) void {
        self.current += 1;
    }
};
