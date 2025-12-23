const std = @import("std");
const Stmt = @import("Parser.zig").Stmt;
const Span = @import("main.zig").Span;
const Expr = @import("Parser.zig").Expr;
const TypeAnnotation = @import("Parser.zig").TypeAnnotation;
const c = @import("llvm.zig").c;

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

pub const Symbol = union(enum) {
    variable: VariableSymbol,
    function: FunctionSymbol,
    extern_fn: ExternFnSymbol,

    pub const VariableSymbol = struct {
        span: Span,
        ref_count: *u32,
        mutable: bool,
        name: []const u8,
        value: Expr,
        type: TypeAnnotation,
        //llvm_value: ?c.LLVMValueRef,
        //llvm_type: ?c.LLVMTypeRef,
    };

    pub const FunctionSymbol = struct {
        span: Span,
        ref_count: *u32,
        name: []const u8,
        //param_types: []TypeAnnotation,
        ret_type: TypeAnnotation,
        //llvm_param_types: ?[]c.LLVMTypeRef,
        //llvm_ret_type: ?c.LLVMTypeRef,        
    };

    pub const ExternFnSymbol = struct {
        span: Span,
        ref_count: *u32,
        name: []const u8,
        param_type: TypeAnnotation,
        ret_type: TypeAnnotation,
        //llvm_param_types: ?[]c.LLVMTypeRef,
        //llvm_ret_type: ?c.LLVMTypeRef,
    };
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

pub const AnalyzedNode = union(enum) {
    stmt: AnalyzedStmt,
    call: AnalyzedFuncCall,
    literal: AnalyzedLiteral,  

    pub const AnalyzedStmt = struct {
        parsed: Stmt,
        symbol: Symbol,  
    };

    pub const AnalyzedFuncCall = struct {
        parsed: Stmt,
        symbol: Symbol,
        ret_type: TypeAnnotation,  
    };

    pub const AnalyzedLiteral = struct {
        parsed: Stmt,
        lit_type: TypeAnnotation,  
    };
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

    pub fn analyze(self: *Analyzer) !?[]AnalyzedNode {
        try self.symbol_table.enterScope(); // 'program scope'

        var nodes: std.array_list.Aligned(AnalyzedNode, null) = .empty;
        for (self.ast) |top_stmt| {
            try nodes.append(self.alloc,
            try self.traverseTopStmt(top_stmt) orelse return null);
        }

        var it = self.symbol_table.current_scope.*.symbols.iterator();
        while (it.next()) |entry| {
            // no global variables yet
            switch (entry.value_ptr.*) {
                .function => |f| {
                    if (f.ref_count.* == 0) {
                        if (std.mem.eql(u8, f.name, "main")) break;
                        try collectWarning(
                            self,
                            "#09412 unused function",
                            f.name,
                            f.span
                        );
                    }
                },
                .extern_fn => |ef| {
                    if (ef.ref_count.* == 0) {
                        // hardcoding like this for now...
                        if (std.mem.eql(u8, ef.name, "putchar")) break;
                        try collectWarning(
                            self,
                            "#45902 unused external function",
                            ef.name,
                            ef.span
                        );
                    }
                },
                else => {},
            }
        }

        self.symbol_table.exitScope();

        return try nodes.toOwnedSlice(self.alloc);
    }

    fn traverseTopStmt(self: *Analyzer, stmt: Stmt) !?AnalyzedNode {
        return switch (stmt) {
            .fn_decl => |s| {
                return try checkFunction(self, s);
            },
            .extern_fn_decl => |s| {
                return try checkExternDecl(self, s);
            },
            else => {
                return null;
            },
        };
    }

    fn checkExternDecl(self: *Analyzer, stmt: Stmt.ExternFnDecl) !AnalyzedNode {
        if (self.symbol_table.lookupItem(stmt.name.literal.value) != null) {
            try collectError(
                self,
                "#45421 external function with this name is already declared",
                stmt.name.literal.value,
                stmt.name.literal.span,
            );
        } 
        
        const ptr = try self.alloc.create(u32);
        ptr.* = 0;

        const extern_decl_symbol = Symbol{ .extern_fn = .{
            .name = stmt.name.literal.value,
            .ref_count = ptr,
            .span = stmt.name.literal.span,
            .ret_type = stmt.ret_type,
            .param_type = stmt.arg_type,
        }};
        
        try self.symbol_table.addItemToScope(
            stmt.name.literal.value,
            extern_decl_symbol
        );

        return AnalyzedNode{ .stmt = .{
            .parsed = Stmt{ .extern_fn_decl = stmt },
            .symbol = extern_decl_symbol,
        }};
    }

    fn checkFunction(self: *Analyzer, func: Stmt.FnDecl) !AnalyzedNode {
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

        const fn_symbol = Symbol{ .function = .{
            .name = func.name.literal.value,
            .span = func.name.literal.span,
            .ref_count = ptr,
            .ret_type = func.ret_type,
        }};
        
        try self.symbol_table.addItemToScope(func.name.literal.value, fn_symbol);

        return AnalyzedNode{ .stmt = .{
            .parsed = Stmt{ .fn_decl = func },
            .symbol = fn_symbol,
        }};
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
                lookup.?.variable.ref_count.* += 1;
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
            switch (entry.value_ptr.*) {
                .variable => |v| {
                    if (v.ref_count.* == 0) {
                        try collectWarning(
                            self,
                            "#14398 unused variable",
                            v.name,
                            v.span,
                        );
                    }
                },
                else => {},
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
            .unary => |e| {
                try checkExpr(self, e.*.operand);
            },
            .grouping => |e| {
                try checkExpr(self, e.*);
            },
            .func_call => |e| {
                try checkExpr(self, e.*.func_name);
            },
            else => {},
        }
    }

    // TODO distinguish between variables, functions and extern functions
    // so we can give different error msgs
    fn checkLiteralExpr(self: *Analyzer, lit_expr: Expr.Literal) !void {
        if (lit_expr.type == .identifier) {
            const lookup = self.symbol_table.lookupItem(lit_expr.value);
            if (lookup == null) {
                // hardcoding for now...
                if (std.mem.eql(u8, lit_expr.value, "println")) return;
                try collectError(
                    self,
                    "#5913 symbol not found",
                    lit_expr.value,
                    lit_expr.span
                );
            } else {
                switch (lookup.?) {
                    .variable => lookup.?.variable.ref_count.* += 1,
                    .function => lookup.?.function.ref_count.* += 1,
                    .extern_fn => lookup.?.extern_fn.ref_count.* += 1,
                }
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

        try checkExpr(self, decl.value);

        const ptr = try self.alloc.create(u32);
        ptr.* = 0;

        const decl_symbol = Symbol{ .variable = .{
            .ref_count = ptr,
            .name = decl.name.literal.value,
            .span = decl.name.literal.span,
            .type = decl.var_type,
            .mutable = decl.mutable,
            .value = decl.value,
        }};

        try self.symbol_table.addItemToScope(decl.name.literal.value, decl_symbol);

        //return AnalyzedNode{ .stmt = .{
        //    .parsed = Stmt{ .var_decl = decl },
        //    .symbol = decl_symbol,
        //}};
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
