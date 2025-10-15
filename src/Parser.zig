const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const main = @import("main.zig");
const Symbol = @import("Analyzer.zig").Symbol;

const Token = Tokenizer.Token;
const TokenType = Tokenizer.TokenType;

const print = std.debug.print;

const ParseError = error{
    IndexError,
    NotPutChar,
    MissingType,
    MissingColon,
};

pub const Type = union(enum) {
    c_int,
    void,
    u8,
    str: []const u8,
};

// the nullable fields get filled in during semantic analysis
pub const Node = union(enum) {
    program: ProgramNode,
    identifier: IdentifierNode,
    literal: LiteralNode,
    function_call: FunctionCallNode,
    extern_fn_decl: ExternFnDeclarationNode,
    function_decl: FunctionDeclarationNode,
    variable_decl: VariableDeclarationNode,
    variable_upd: VariableUpdateNode,

    // do we need to deinit these arraylists as well?
    pub const ProgramNode = struct {
        program_ast: std.array_list.Aligned(*Node, null),
    };

    pub const IdentifierNode = struct {
        name: []const u8,
        token: Token,
        
    };

    // replace type of value with Type? and is it a pointer to Type then?
    pub const LiteralNode = struct {
        value: []const u8,
        token: Token,
    };

    pub const FunctionCallNode = struct {
        func_identifier: *Node,
        func_argument: *Node, // literal

        func_symbol: ?*Symbol,
        ret_type: ?Type,
    };

    pub const ExternFnDeclarationNode = struct {
        func_identifier: *Node,
        arg_type: Type,
        ret_type: Type,
        arg_token: Token,
        ret_token: Token,

        extern_fn_symbol: ?*Symbol,
    };

    pub const FunctionDeclarationNode = struct {
        name: []const u8,
        name_token: Token,
        body: std.array_list.Aligned(*Node, null),
        // parameters: []*Node,
        // ret_type: Type,

        func_symbol: ?*Symbol,
    };

    pub const VariableDeclarationNode = struct {
        name: *Node,
        value: *Node,
        var_token: Token,
        mutable: bool,
        type: Type,

        var_symbol: ?*Symbol,
    };

    pub const VariableUpdateNode = struct {
        name: *Node,
        op: *Node,
        value: *Node,
        var_token: Token,

        var_upd_symbol: ?*Symbol,
        type: ?Type,
    };
};

pub const AstPrinter = struct {
    pub fn printAst(ast: std.array_list.Aligned(*Node, null)) void {
        for (ast.items) |node| {
            traverseNode(node.*);
        }
        print("\n", .{});
    }

    // TODO: consider indentations
    fn traverseNode(node: Node) void {
        switch (node) {
            .extern_fn_decl => {},
            .function_decl => |decl| {
                print("(", .{});
                print("Function Declaration | ", .{});
                print("Name: {s} | ", .{decl.name});
                print("Body: ", .{});

                for (decl.body.items) |item| {
                    traverseNode(item.*);
                }

                print(")", .{});
            },
            .variable_decl => |decl| {
                print("(", .{});
                print("Variable Declaration | ", .{});

                print("Name: ", .{});
                traverseNode(decl.name.*);

                print("Value: ", .{});
                traverseNode(decl.value.*);

                print(")", .{});
            },
            .identifier => |ident| {
                print("Identifier: {s} | ", .{ident.name});
            },
            .literal => |lit| {
                print("Literal: {s} | ", .{lit.value});  
            },
            else => {},
        }
    }
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

    // TODO: replace with deinit in Analyzer and remove this
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

        //AstPrinter.printAst(self.ast);

        return Node{ .program = .{
            .program_ast = self.ast,
        } };
    }

    fn parseToken(self: *Parser) !Node {
        const current_token_type = getCurrentToken(self).token_type;
        var node: Node = undefined;

        switch (current_token_type) {
            .EXTERN => {
                node = try externFnDeclaration(self);
            },
            .FN => {
                if (peekType(self) == .IDENTIFIER) {
                    node = try functionDeclaration(self);
                }
            },
            // no 'global' block yet for globals
            .IDENTIFIER => {
                node = try variableUpdate(self);
            },
            else => {},
        }

        return node;
    }

    fn variableUpdate(self: *Parser) !Node {
        const var_token = getCurrentToken(self);
        
        const var_ident = identifier(self);
        const ident_ptr = try self.allocator.create(Node);
        ident_ptr.* = var_ident;

        const op = literal(self);
        const op_ptr = try self.allocator.create(Node);
        op_ptr.* = op;

        const lit = literal(self);
        const lit_ptr = try self.allocator.create(Node);
        lit_ptr.* = lit;

        return Node{ .variable_upd = .{
            .name = ident_ptr,
            .op = op_ptr,
            .value = lit_ptr,
            .var_token = var_token,
            .type = null,
            .var_upd_symbol = null,
        }};
    }

    // TODO: a general statement, that has a semicolon at the end of it,
    // then we can check that there's a semicolon at (almost) every line

    fn variableDeclaration(self: *Parser) !Node {
        advance(self);

        var mutable = false;
        if (getCurrentToken(self).token_type == .MUT) {
            mutable = true;
            advance(self);
        }
        const var_token = getCurrentToken(self);
        
        const ident = identifier(self);
        const ident_ptr = try self.allocator.create(Node);
        ident_ptr.* = ident;

        advance(self);
        const token_type = getCurrentToken(self).token_type;
        var var_type: Type = undefined;

        switch (token_type) {
            .C_INT => { var_type = .c_int; },
            else => {},
        }
        
        advance(self);
        advance(self);

        const val = literal(self);
        const val_ptr = try self.allocator.create(Node);
        val_ptr.* = val;

        while (getCurrentToken(self).token_type != .SEMICOLON) advance(self);
        advance(self);

        return Node{ .variable_decl = .{
            .name = ident_ptr,
            .value = val_ptr,
            .var_token = var_token,
            .mutable = mutable,
            .type = var_type,
            .var_symbol = null,
        } };
    }

    fn functionDeclaration(self: *Parser) !Node {
        // CHECK: do we have to deinit this? we'll see!
        var func_body: std.array_list.Aligned(*Node, null) = .empty;

        advance(self); // eat fn token
        const name = self.tokens.items[self.current].lexeme;
        const name_token = self.tokens.items[self.current];

        while (self.tokens.items[self.current].token_type != .LEFT_BRACE) advance(self);
        advance(self); // eat left brace

        var curr_type = self.tokens.items[self.current].token_type;

        // currently only variable declarations and function calls
        while (curr_type != .RETURN) {
            switch (curr_type) {
                .LET => {
                    const curr = try variableDeclaration(self);
                    const curr_ptr = try self.allocator.create(Node);
                    curr_ptr.* = curr;
                    try func_body.append(self.allocator, curr_ptr);
                },
                .IDENTIFIER => {
                    const curr = try functionCall(self);
                    const curr_ptr = try self.allocator.create(Node);
                    curr_ptr.* = curr;
                    try func_body.append(self.allocator, curr_ptr);
                },
                else => {},
            }
            curr_type = self.tokens.items[self.current].token_type;
        }

        // hardcoded return type
        while (self.tokens.items[self.current].token_type != .RIGHT_BRACE) advance(self);
        if (!isAtEnd(self)) advance(self);

        return Node{ .function_decl = .{
            .body = func_body,
            .name = name,
            .name_token = name_token,
            .func_symbol = null,
        } };
    }

    fn peekType(self: *Parser) TokenType {
        return self.tokens.items[self.current + 1].token_type;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.current == self.tokens.items.len - 1;
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
        //const arg_type = argumentType(self);
        advance(self);
        advance(self);
        const arg_token = self.tokens.items[self.current];
        advance(self);
        
        advance(self);
        advance(self);
        //const ret_type = returnType(self);
        const ret_token = self.tokens.items[self.current];
        
        advance(self); // eat semicolon // TODO: decouple this sometime?

        return Node{ .extern_fn_decl = .{
            // hardcoded c_int for now
            .arg_type = .c_int,
            .ret_type = .c_int,
            .func_identifier = ident_ptr,
            .arg_token = arg_token,
            .ret_token = ret_token,
            .extern_fn_symbol = null,
        } };
    }

    // TODO: remove
    fn argumentType(self: *Parser) Type {
        // eat the three tokens
        advance(self);
        advance(self);
        advance(self);
        return .c_int;
    }

    // TODO: remove
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

        return Node{ .function_call = .{
            .func_argument = arg_lit_ptr,
            .func_identifier = ident_ptr,
            .func_symbol = null,
            .ret_type = null,
        } };
    }

    // Identifier : \String of characters not reserved as a token\
    fn identifier(self: *Parser) Node {
        const name = self.tokens.items[self.current].lexeme;
        const token = self.tokens.items[self.current];
        const ident = Node{ .identifier = .{
            .name = name,
            .token = token,
        } };

        advance(self);

        return ident;
    }

    // TODO: this is the same as literal() so replace and remove
    fn arg_literal(self: *Parser) Node {
        const lexeme = self.tokens.items[self.current].lexeme;
        const token = self.tokens.items[self.current];

        advance(self);

        return Node{ .literal = .{
            .value = lexeme,
            .token = token,
        } };
    }

    // Literal : \Any direct source code value\
    fn literal(self: *Parser) Node {
        const lexeme = self.tokens.items[self.current].lexeme;
        const token = self.tokens.items[self.current];

        const lit = Node{ .literal = .{
            .value = lexeme,
            .token = token,
        } };

        advance(self);

        return lit;
    }

    // TODO: add token type to advance so we can check
    // correct type. or some parameter to give to it,
    // for better errors and better code readability
    fn advance(self: *Parser) void {
        self.current += 1;
    }
};
