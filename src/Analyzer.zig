const std = @import("std");
const Stmt = @import("Parser.zig").Stmt;
const Span = @import("main.zig").Span;
const Expr = @import("Parser.zig").Expr;
const print = std.debug.print;

const Diagnostic = struct {
    msg: ?[]const u8,
    severity: enum { err, warn, info },
    //stmt: Stmt,
    value: []const u8,
    span: Span,
};

fn initDiagnostics() std.array_list.Aligned(Diagnostic, null) {
    const diagnostics: std.array_list.Aligned(Diagnostic, null) = .empty;
    return diagnostics;
}

pub const SymbolType = enum {
    variable,
    function,
    externfn,   
};

pub const Symbol = struct {
    span: Span,
    value: []const u8,
    mutable: ?bool,
    symbol_type: SymbolType,
    ref_count: *u32,
    stmt: Stmt,
};

pub const Scope = struct {
    symbols: std.StringHashMap(Symbol),
    parent: ?*Scope,  
};

pub const SymbolTable = struct {
    current_scope: *Scope,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !SymbolTable {
        const scope = try allocator.create(Scope);
        scope.* = Scope{
            .parent = null,
            .symbols = std.StringHashMap(Symbol).init(allocator),  
        };
        return SymbolTable{
            .current_scope = scope,
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

        old_scope.symbols.deinit();
        self.allocator.destroy(old_scope);
    }

    pub fn lookupItem(self: *SymbolTable, name: []const u8) ?Symbol {
        var scope: ?*Scope = self.current_scope;

        //print("looking up {s}\n", .{name});
        
        while (scope) |current| {
            if (current.symbols.get(name)) |symbol| {
                //print("lookup succeeded.\n", .{});
                return symbol;
            }
            scope = current.parent;
        }

        //print("lookup failed. for initializations this is good.\n", .{});
        
        return null;
    }

    pub fn lookupItemCurrentScope(self: *SymbolTable, name: []const u8) !void {
        const scope = self.current_scope.*;
        if (scope.symbols.get(name)) |symbol| {
            return symbol;
        }
        return null;
    }

    pub fn addItemToScope(self: *SymbolTable, name: []const u8, item: Symbol) !void {
        try self.current_scope.*.symbols.put(name, item);
    }
};

pub const Analyzer = struct {
    ast: []Stmt,
    symbol_table: SymbolTable,
    alloc: std.mem.Allocator,
    diagnostics: std.array_list.Aligned(Diagnostic, null),

    pub fn init(ast: []Stmt, allocator: std.mem.Allocator) !Analyzer {
        return Analyzer{
            .ast = ast,
            .symbol_table = try SymbolTable.init(allocator),
            .alloc = allocator,
            .diagnostics = initDiagnostics(),
        };
    }

    pub fn analyze(self: *Analyzer) !void {
        try self.symbol_table.enterScope(); // 'program scope'
        
        for (self.ast) |top_stmt| {
            try self.traverseTopStmt(top_stmt);
        }

        self.symbol_table.exitScope();
    }

    fn traverseTopStmt(self: *Analyzer, stmt: Stmt) !void {
        switch (stmt) {
            .fn_decl => |s| {
                try checkFunction(self, s);
            },
            .extern_fn_decl => |s| {
                try checkExternDecl(self, s);
            },
            else => {},
        }
    }

    fn checkExternDecl(self: *Analyzer, stmt: Stmt.ExternFnDecl) !void {
        const val = stmt.name.literal.value;
        const ptr = try self.alloc.create(u32);
        ptr.* = 0;
        
        const ext_decl_symbol = Symbol{
            .mutable = null,
            .span = stmt.name.literal.span,
            .symbol_type = .externfn,
            .value = val,
            .ref_count = ptr,
            .stmt = Stmt{ .extern_fn_decl = stmt },
        };
        try self.symbol_table.addItemToScope(val, ext_decl_symbol);
    }

    fn checkFunction(self: *Analyzer, func: Stmt.FnDecl) !void {
        if (self.symbol_table.lookupItem(func.name.literal.value) != null) {
            try collectError(
                self,
                "#24323 function with this name is already declared",
                func.name.literal.value,
                func.name.literal.span
            );
        }
        
        try checkBlock(self, func.fn_body);

        const ptr = try self.alloc.create(u32);
        ptr.* = 0;
        
        const fn_symbol = Symbol{
            .mutable = null,
            .ref_count = ptr,
            .span = func.name.literal.span,
            .stmt = Stmt{ .fn_decl = func },
            .value = func.name.literal.value,
            .symbol_type = .function,   
        };
        try self.symbol_table.addItemToScope(func.name.literal.value, fn_symbol);
    }

    fn checkRetStmt(self: *Analyzer, ret_stmt: Stmt.RetStmt) !void {
        if (ret_stmt.value.literal.type == .identifier) {
            const lookup = self.symbol_table.lookupItem(ret_stmt.value.literal.value);
            if (lookup == null) {
                try collectError(
                    self,
                    "#9852 variable not found",
                    ret_stmt.value.literal.value,
                    ret_stmt.value.literal.span
                );
            } else {
                lookup.?.ref_count.* += 1;
            }
        }
    }

    fn checkBlock(self: *Analyzer, block: []Stmt) !void {
        try self.symbol_table.enterScope();

        for (block) |stmt| {
            switch (stmt) {
                .expr_stmt => |s| {
                    try checkExpr(self, s.expr);
                },
                .extern_fn_decl => |s| {
                    try collectError(
                        self,
                        "#09134 cannot define external function in block",
                        s.name.literal.value,
                        s.name.literal.span
                    );
                },
                .fn_decl => |s| {
                    try collectError(
                        self,
                        "#41310 nested functions not supported",
                        s.name.literal.value,
                        s.name.literal.span
                    );
                },
                .if_stmt => |s| {
                    try checkExpr(self, s.condition);
                    try checkBlock(self, s.if_body);
                },
                .ret_stmt => |s| {
                    try checkRetStmt(self, s);
                },
                .var_decl => |s| {
                    try checkVarDecl(self, s);
                },
                .while_loop => |s| {
                    try checkExpr(self, s.cond);
                    try checkBlock(self, s.body);
                },
                .error_node => {},
            }
        }

        var it = self.symbol_table.current_scope.*.symbols.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.ref_count.* == 0) {
                switch (entry.value_ptr.symbol_type) {
                    .variable => {
                        try collectWarning(
                            self,
                            "#4111 unused variable",
                            entry.value_ptr.value,
                            entry.value_ptr.span
                        );
                    },
                    .function => {
                        try collectWarning(
                            self,
                            "#841 unused function",
                            entry.value_ptr.value,
                            entry.value_ptr.span
                        );
                    },
                    .externfn => {
                        try collectWarning(
                            self,
                            "#109431 unused external function",
                            entry.value_ptr.value,
                            entry.value_ptr.span
                        );
                    },
                }
            }
        }

        self.symbol_table.exitScope();
    }

    fn checkExpr(self: *Analyzer, expr: Expr) !void {
        switch (expr) {
            .literal => |e| {
                try checkLiteralExpr(self, e);
            },
            .binary => |e| {
                try checkExpr(self, e.*.left);
                try checkExpr(self, e.*.right);
            },
            else => {},
        }
    }

    fn checkLiteralExpr(self: *Analyzer, lit_expr: Expr.Literal) !void {
        if (lit_expr.type == .identifier) {
            //print("#48239 ---{s}---\n", .{lit_expr.value});
            const lookup = self.symbol_table.lookupItem(lit_expr.value);
            if (lookup == null) {
                try collectError(
                    self,
                    "#5913 variable not found",
                    lit_expr.value,
                    lit_expr.span
                );
            } else {
                lookup.?.ref_count.* += 1;
            }
        }
    }
    
    fn checkVarDecl(self: *Analyzer, decl: Stmt.VariableDecl) !void {
        const lookup = self.symbol_table.lookupItem(decl.name.literal.value);
        if (lookup != null) {
            try collectError(
                self,
                "#59013 variable with this name already exists",
                decl.name.literal.value,
                decl.name.literal.span
            );
        }

        // TODO generalize for different expr types
        const val = decl.value.literal.value;
        
        if (decl.value.literal.type == .identifier) {
            const lookup_value = self.symbol_table.lookupItem(val);
            if (lookup_value == null) {
                try collectError(
                    self,
                    "#1623 variable not found",
                    val,
                    decl.value.literal.span
                );
            } else {
                lookup_value.?.ref_count.* += 1;
            }
        }

        const ptr = try self.alloc.create(u32);
        ptr.* = 0;

        //print("#453529 variable name: '{s}'\n", .{decl.name.literal.value});
        
        const decl_symbol = Symbol{
            .mutable = decl.mutable,
            .span = decl.name.literal.span,
            .value = decl.name.literal.value,
            .symbol_type = .variable,
            .ref_count = ptr,
            .stmt = Stmt{ .var_decl = decl },
        };

        try self.symbol_table.addItemToScope(decl.name.literal.value, decl_symbol);
    }

    fn collectError(self: *Analyzer, msg: ?[]const u8, val: []const u8, span: Span) !void {
        const diag = Diagnostic{
            .msg = msg,
            .severity = .err,
            .span = span,
            .value = val,
        };
        try self.diagnostics.append(self.alloc, diag);
    }

    fn collectWarning(self: *Analyzer, msg: ?[]const u8, val: []const u8, span: Span) !void {
        const diag = Diagnostic{
            .msg = msg,
            .severity = .warn,
            .value = val,
            .span = span,
        };
        try self.diagnostics.append(self.alloc, diag);
    }
};
