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
    literal: LiteralNode,
    function_call: FunctionCallNode,

    pub const ProgramNode = struct {
        program_ast: std.array_list.Aligned(*Node, null),
    };

    pub const IdentifierNode = struct {
        name: []const u8,
    };

    pub const LiteralNode = struct {
        value: []const u8,
    };

    pub const FunctionCallNode = struct {
        func_identifier: *Node,
        func_argument: *Node, // type: identifier
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

    pub fn deinit(self: *Parser) void {
        self.ast.deinit(self.allocator);
    }

    pub fn parseProgram(self: *Parser) !Node {
        while (!isAtEnd(self)) {
            const func_call = try functionCall(self);
            const call_ptr = try self.allocator.create(Node);
            call_ptr.* = func_call;
            
            try self.ast.append(self.allocator, call_ptr);
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

        advance(self); // eat left_paren

        const arg_lit = arg_literal(self);
        const arg_lit_ptr = try self.allocator.create(Node);
        arg_lit_ptr.* = arg_lit;

        advance(self); // eat right_paren
        advance(self); // eat semicolon TODO: later have 'statement' do this

        return Node{.function_call = .{
            .func_argument = arg_lit_ptr,
            .func_identifier = ident_ptr, 
        }};
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

    fn arg_literal(self: *Parser) Node {
        const lexeme = self.tokens.items[self.current].lexeme;

        advance(self);

        return Node{.literal = .{
            .value = lexeme,
        }};
    }

    // Literal : \Any direct source code value\
    fn literal(self: *Parser) Node {
        const lexeme = self.tokens.items[self.current].lexeme;
        const lit = Node{.literal = .{
            .value = lexeme,
        }};
        
        advance(self);
        
        return lit;
    }

    // TODO: add token type to advance so we can check
    // correct type
    fn advance(self: *Parser) void {
        self.current += 1;
    }
};
