const std = @import("std");
const Stmt = @import("Parser.zig").Stmt;
const Token = @import("Tokenizer.zig").Token;
const TokenType = @import("Tokenizer.zig").TokenType;
const TypeAnnotation = @import("Parser.zig").TypeAnnotation;
const Span = @import("main.zig").Span;
const c = @import("llvm.zig").c;
const print = std.debug.print;

const AnalysisError = error{
    SymbolAlreadyDefined,
    VariableOutOfScope,
    VariableNotDefined,
};

pub const SymbolType = enum {
    variable,
    function,
    parameter,
    argument,
    // etc...
};

pub const Symbol = struct {
    span: Span,
    value: []const u8, // literal value
    data_type: TypeAnnotation, // the data type or return type (if is a function)
    mutable: bool,
    symbol_type: SymbolType, // variable, function, parameter, etc.
    llvm_value: ?*c.LLVMValueRef,
};

pub const Scope = struct {
    symbols: ?std.StringHashMap(Symbol),
    parent: ?*Scope,
};

pub const SymbolTable = struct {
    current_scope: *Scope,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !SymbolTable {
        const scope = Scope{
            .parent = null,
            .symbols = null,  
        };
        const scope_ptr = try allocator.create(Scope);
        scope_ptr.* = scope;
        
        return SymbolTable{
            .current_scope = scope_ptr,
            .allocator = allocator,
        };
    }

    pub fn enterScope(self: *SymbolTable) !void {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope{
            .symbols = std.StringHashMap(Symbol).init(self.allocator),
            .parent = self.current_scope,
        };
        self.current_scope = new_scope;
    }

    pub fn exitScope(self: *SymbolTable) void {
        var old_scope = self.current_scope;
        self.current_scope = old_scope.parent.?;

        old_scope.symbols.?.deinit();
        self.allocator.destroy(old_scope);
    }

    // search for symbol by name, starting from the current scope and
    // going to parent scopes if not found in current
    pub fn lookupItem(self: *SymbolTable, name: []const u8) ?Symbol {
        var scope = self.current_scope.*;

        while (scope.symbols != null) {
            if (scope.symbols.?.get(name)) |symbol| {
                return symbol;
            }
            scope = scope.parent.?.*;
        }
        
        return null;
    }

    pub fn addItemToCurrentScope(self: *SymbolTable, name: []const u8, item: Symbol) !void {
        if (self.current_scope.*.symbols.?.contains(name)) {
           return AnalysisError.SymbolAlreadyDefined;
        }
        try self.current_scope.*.symbols.?.put(name, item);
    }
};

pub const Analyzer = struct {
    ast: []Stmt,
    symbol_table: SymbolTable,
    allocator: std.mem.Allocator,

    pub const Result = struct {
        ast: []Stmt,
        symbol_table: SymbolTable,
    };

    pub fn init(ast: []Stmt, allocator: std.mem.Allocator) !Analyzer {
        return Analyzer{
            .ast = ast,
            .symbol_table = try SymbolTable.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn analyzeAst(self: *Analyzer) !Result {
        for (self.ast.program.program_ast.items) |node| {
            try traverseTopStmt(self, node.*);
        }

        return Result{
            .ast = self.ast,
            .symbol_table = self.symbol_table,
        };
    }

    fn traverseTopStmt(self: *Analyzer, stmt: Stmt) !void {
        switch (stmt) {
            .function_decl => |decl| {
                try self.symbol_table.enterScope();
                try checkFunction(self, decl);
                self.symbol_table.exitScope();    
            },
            .variable_upd => |upd| {
                try checkVariableUpdate(self, upd);
            },
            else => {},
        }
    }

    fn checkVariableUpdate(self: *Analyzer, node: Node.VariableUpdateNode) !void {
        const variable = self.symbol_table.lookupItem(node.name.*.identifier.name);
        if (variable == null) {
            const line = node.var_token.line;
            const col = node.var_token.col;
            print("{d}:{d} | ", .{line, col});
            return AnalysisError.VariableOutOfScope;
        }
    }

    fn checkFunction(self: *Analyzer, node: Node.FunctionDeclarationNode) !void {
        for (node.body.items) |body_node| {
            switch (body_node.*) {
                .variable_decl => |decl| {
                    const var_decl = Symbol{
                        .llvm_value = null,
                        .mutable = decl.mutable,
                        .value = decl.value.*.literal.value,
                        .token = decl.var_token,
                        .data_type = decl.type,
                        .symbol_type = .variable,
                    };
                    const name = decl.name.*.identifier.name;
                    try self.symbol_table.addItemToCurrentScope(name, var_decl);
                },
                else => {},
            }
        }

        // etc...
    }

    // support simple binary comparisons
    fn checkIfStatement(self: *Analyzer, node: Node.IfStatementNode) !void {
        const cond = node.condition.*.expression.expr;
        for (cond.items) |token| {
            switch (token.token_type) {
                .IDENTIFIER => {
                    const name = self.symbol_table.lookupItem(token.lexeme);
                    if (name == null) return AnalysisError.VariableNotDefined; 
                }
            }
        }
        
    }
};
