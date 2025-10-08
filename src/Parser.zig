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
        program_ast: std.array_list.Aligned(*Node, null) = .empty,
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
    ast: std.array_list.Aligned(*Node, null),

    pub fn init(tokens_list: std.array_list.Aligned(Token, null), allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens_list,
            .current = 0,
            .allocator = allocator,
            .ast = .empty,
        };
    }

    // parse multiple putchar()-calls
    pub fn parseTokens(self: *Parser) !Node {
        while (!isAtEnd(self)) {
            const token = getCurrentToken(self);
            
            switch (token.token_type) {
                .IDENTIFIER => {
                    const func_call = try functionCall(self);
                    const call_ptr = try self.allocator.create(Node);
                    call_ptr.* = func_call;

                    try self.ast.append(self.allocator, call_ptr);
                },
                .SEMICOLON, .EOF => advance(self),
                else => {},
            }

            advance(self);
        }

        return Node {.program = .{
            .program_ast = self.ast,
        }};
    }

    fn isAtEnd(self: *Parser) bool {
        return self.current >= self.tokens.items.len;
    }

    fn getCurrentToken(self: *Parser) Token {
        const current_token = self.tokens.items[self.current];
        return current_token;
    }

    // FunctionCall : Identifier "(" Argument ")"
    fn functionCall(self: *Parser) !Node {
        const ident = identifier(self);
        const ident_ptr = try self.allocator.create(Node);
        ident_ptr.* = ident;

        const arg = try argument(self);
        const arg_ptr = try self.allocator.create(Node);
        arg_ptr.* = arg;

        return Node{ .function_call = .{
            .call_argument = arg_ptr,
            .identifier = ident_ptr,
        } };
    }

    // Identifier : \String of characters not reserved as a token\
    fn identifier(self: *Parser) Node {
        const name = self.tokens.items[self.current].lexeme;

        const ident = Node{ .identifier = .{
            .name = name,
        } };

        advance(self);

        return ident;
    }

    // Argument : Literal
    fn argument(self: *Parser) !Node {
        advance(self); // skip left_paren
        
        const lit = literal(self);
        const lit_ptr = try self.allocator.create(Node);
        lit_ptr.* = lit;

        advance(self); // skip right_paren
        
        const arg = Node{ .argument = .{
            .argument_literal = lit_ptr,
        } };

        return arg;
    }

    // Literal : \Any direct source code value\
    fn literal(self: *Parser) Node {
        const lexeme = self.tokens.items[self.current].lexeme;
        return Node{ .literal = .{
            .value = lexeme,
        } };
    }

    fn advance(self: *Parser) void {
        self.current += 1;
    }
};
