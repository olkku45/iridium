const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const main = @import("main.zig");

const Token = Tokenizer.Token;
const TokenType = Tokenizer.TokenType;

const Error = error{
    IndexError,
    NotPutChar,
};

pub const Type = union(enum) {
    c_int,
    void,
    u8,
    str: []const u8,
};

pub const Node = union(enum) {
    program: ProgramNode,
    identifier: IdentifierNode,
    literal: LiteralNode,
    function_call: FunctionCallNode,
    extern_fn_decl: ExternFnDeclarationNode,
    function_decl: FunctionDeclarationNode,
    return_statement: ReturnStatementNode,

    // TODO: do we need to deinit these arraylists as well?
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
        func_argument: *Node, // type: literal
    };

    pub const ExternFnDeclarationNode = struct {
        func_identifier: *Node,
        arg_type: Type,
        ret_type: Type,
    };

    pub const FunctionDeclarationNode = struct {
        name: []const u8,
        body: std.array_list.Aligned(*Node, null),
        // parameters: []*Node,
        // ret_type: Type,
    };

    pub const ReturnStatementNode = struct {
        ret_type: Type,
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
            const node = try parseToken(self);
            const node_ptr = try self.allocator.create(Node);
            node_ptr.* = node;
            
            try self.ast.append(self.allocator, node_ptr);
        }
        
        return Node {.program = .{
            .program_ast = self.ast,
        }};
    }

    fn parseToken(self: *Parser) !Node {
        const current_token_type = getCurrentToken(self).token_type;
        var node: Node = undefined;
        
        switch (current_token_type) {
            .EXTERN => {
                node = try externFnDeclaration(self);
            },
            // TODO: remove
            .IDENTIFIER => {
                node = try functionCall(self);
            },
            .FN => {
                if (peekType(self) == .IDENTIFIER) {
                    node = try functionDeclaration(self);
                }
            },
            else => {},
        }

        return node;
    }

    fn functionDeclaration(self: *Parser) !Node {
        // TODO: do we have to deinit this? we'll see!
        var func_body: std.array_list.Aligned(*Node, null) = .empty;
        
        advance(self); // eat fn token
        const name = self.tokens.items[self.current].lexeme;

        while (self.tokens.items[self.current].token_type != .LEFT_BRACE) advance(self);

        advance(self); // eat left brace
        
        while (self.tokens.items[self.current].token_type != .RETURN) {
            // assumption that we only have function calls
            const current = try functionCall(self);
            const current_ptr = try self.allocator.create(Node);
            current_ptr.* = current;
            try func_body.append(self.allocator, current_ptr);
        }

        // hardcoded return type
        while (self.tokens.items[self.current].token_type != .RIGHT_BRACE) advance(self);

        return Node{ .function_decl = .{
            .body = func_body,
            .name = name,
        }};
    }

    fn peekType(self: *Parser) TokenType {
        return self.tokens.items[self.current + 1].token_type;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.current + 1 == self.tokens.items.len;
    }

    fn getCurrentToken(self: *Parser) Token {
        const current_token = self.tokens.items[self.current];
        return current_token;
    }

    // Extern fn decl : "extern" "fn" Identifier "(" Arg_Type ")" "->" Ret_Type ";"
    fn externFnDeclaration(self: *Parser) !Node {
        // eat keywords
        advance(self);
        advance(self);
        
        const ident = identifier(self);
        const ident_ptr = try self.allocator.create(Node);
        ident_ptr.* = ident;

        advance(self);
        const arg_type = argumentType(self);
        advance(self);
        advance(self); 
        const ret_type = returnType(self);
        advance(self); // eat semicolon // TODO: decouple this sometime?

        return Node{.extern_fn_decl = .{
            .arg_type = arg_type,
            .ret_type = ret_type,
            .func_identifier = ident_ptr,
        }};
    }

    // README: hardcoded c_int for now
    fn argumentType(self: *Parser) Type {
        // eat the three tokens
        advance(self);
        advance(self);
        advance(self);
        return .c_int;
    }

    // README: hardcoded c_int for now
    fn returnType(self: *Parser) Type {
        advance(self);
        return .c_int;
    }

    // FunctionCall : Identifier "(" Argument ")" ";"
    fn functionCall(self: *Parser) !Node {
        const ident = identifier(self);
        const ident_ptr = try self.allocator.create(Node);
        ident_ptr.* = ident;

        advance(self); // eat left_paren

        const arg_lit = arg_literal(self);
        const arg_lit_ptr = try self.allocator.create(Node);
        arg_lit_ptr.* = arg_lit;

        advance(self); // eat right_paren
        advance(self); // eat semicolon TODO: later have 'statement' do this?

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

    // TODO: this is the same as literal() so replace and remove
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
