const std = @import("std");
const Span = @import("main.zig").Span;
const Stmt = @import("Parser.zig").Stmt;
const Expr = @import("Parser.zig").Expr;
const TypeAnnotation = @import("Parser.zig").TypeAnnotation;
const c = @import("llvm.zig").c;
const print = std.debug.print;

const AnalysisError = error{
    DuplicateVariable,
    VariableOutOfScope,
    WrongArgType,  
};

pub const TypeId = union(enum) {
    const id = u32;

    pub const TYPE_VOID: id = 0;
    pub const TYPE_BOOL: id = 1;
    pub const TYPE_I8: id = 2;
    pub const TYPE_I16: id = 3;
    pub const TYPE_I32: id = 4;
    pub const TYPE_I64: id = 5;
    pub const TYPE_U8: id = 6;
    pub const TYPE_U16: id = 7;
    pub const TYPE_U32: id = 8;
    pub const TYPE_U64: id = 9;
    pub const TYPE_F32: id = 10;
    pub const TYPE_F64: id = 11;

    // user defined types...
};

pub const SymbolType = enum {
    variable,
    function,
    parameter,
    argument,
    externfn, 
};

pub const Symbol = struct {
    span: Span,
    value: []const u8,
    mutable: ?bool,
    symbol_type: SymbolType,
    llvm_value: ?*c.LLVMValueRef,
    data_type: TypeAnnotation,
};

pub const Scope = struct {
    symbols: ?std.StringHashMap(Symbol),
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

        old_scope.symbols.?.deinit();
        self.allocator.destroy(old_scope);
    }

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

    pub fn lookupItemCurrentScope(self: *SymbolTable, name: []const u8) ?Symbol {
        const scope = self.current_scope.*;

        if (scope.symbols.?.get(name)) |symbol| {
            return symbol;
        }
        
        return null;
    }

    pub fn addItemToCurrentScope(self: *SymbolTable, name: []const u8, item: Symbol) !void {
        if (self.current_scope.*.symbols.?.contains(name)) {
            try reportError(item);
            return AnalysisError.DuplicateVariable;
        }
        try self.current_scope.*.symbols.?.put(name, item);
    }

    // TODO: remove?
    pub fn reportError(item: Symbol) !void {
        print("Line {d}: Col {d}-{d} | ", .{item.span.line, item.span.start_col, item.span.end_col});
    }
};

pub const Analyzer = struct {
    ast: []Stmt,
    symbol_table: SymbolTable,
    allocator: std.mem.Allocator,

    pub const Result = struct {
        ast: []Stmt,
        st: SymbolTable,
    };

    pub fn init(ast: []Stmt, allocator: std.mem.Allocator) !Analyzer {
        return Analyzer{
            .ast = ast,
            .symbol_table = try SymbolTable.init(allocator),
            .allocator = allocator,  
        };
    }

    pub fn analyzeAst(self: *Analyzer) !Result {
        for (0..self.ast.len) |i| {
            try self.traverseTopStmt(self.ast[i]);
        }
        return Result{
            .ast = self.ast,
            .st = self.symbol_table,  
        };
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
        const decl_symbol = Symbol{
            .llvm_value = null,
            .symbol_type = .externfn,
            .mutable = null,
            .span = stmt.name.literal.span,
            .data_type = stmt.arg_type,
            .value = stmt.name.literal.value,
        };

        try self.symbol_table.addItemToCurrentScope(stmt.name.literal.value, decl_symbol);
    }

    // NOT USED YET
    fn resolveNamedType(named: TypeAnnotation.NamedType) TypeId {
        return switch (named) {
            .primitive => |prim| switch (prim) {
                .void => .TYPE_VOID,
                .bool => .TYPE_BOOL,
                .i8 => .TYPE_I8,
                .i16 => .TYPE_I16,
                .i32 => .TYPE_I32,
                .i64 => .TYPE_I64,
                .u8 => .TYPE_U8,
                .u16 => .TYPE_U16,
                .u32 => .TYPE_U32,
                .u64 => .TYPE_U64,
                .f32 => .TYPE_F32,
                .f64 => .TYPE_F64,
            },
            else => {},
        };
    }

    fn checkFunction(self: *Analyzer, func: Stmt.FnDecl) !void {
        try self.symbol_table.enterScope();
        
        for (func.fn_body) |stmt| {
            switch (stmt) {
                // TODO: 'checkVarDecl()', also this 'decl' could be
                // shortened
                .var_decl => |s| {
                    const decl = Symbol{
                        .llvm_value = null,
                        .mutable = s.mutable,
                        .data_type = s.var_type,
                        .symbol_type = .variable,
                        .value = s.value.literal.value,
                        .span = Span{
                            .start_col = s.value.literal.span.start_col,
                            .end_col = s.value.literal.span.end_col,
                            .line = s.value.literal.span.line,
                            .source_file = null,  
                        },
                    };
                    const name = s.name.literal.value;

                    if (self.symbol_table.lookupItemCurrentScope(name) != null) {
                        reportLiteralError(s.name.literal);
                        return AnalysisError.DuplicateVariable;
                    }
                    try self.symbol_table.addItemToCurrentScope(name, decl);
                },
                .if_stmt => |s| {
                    try checkIfStmt(self, s);
                },
                .ret_stmt => |s| {
                    try checkRetStmt(self, s);  
                },
                // currently just function calls here
                .expr_stmt =>  {
                    //try checkCallExpr(self, s);
                },
                else => {},
            }
        }

        self.symbol_table.exitScope();
    }

    // TODO: this type checking doesn't work
    //fn checkCallExpr(self: *Analyzer, stmt: Stmt.ExprStmt) void {
    //    const call = stmt.expr.func_call.*;
    //    
    //    for (0..call.args.len) |i| {
    //        const fn_type = call.args[i].literal.annotation.?.named.primitive;
    //        const called = self.symbol_table.lookupItem(call.func_name.literal.value);
    //        const called_type = called.?.data_type.named.primitive;

    //        if (fn_type != called_type) {
    //            reportLiteralError(call.func_name.literal);
    //            return AnalysisError.WrongArgType;
    //        }
    //    }        
    //}

    fn checkIfStmt(self: *Analyzer, stmt: Stmt.IfStmt) !void {
        try self.symbol_table.enterScope();

        // currently just binary expressions
        const cond = stmt.condition.binary.*;
        const left = cond.left.literal;
        const right = cond.right.literal;

        if (left.type == .variable) {
            const lookup = self.symbol_table.lookupItem(left.value);
            if (lookup == null) {
                reportLiteralError(left);
                return AnalysisError.VariableOutOfScope;
            }
        }
        if (right.type == .variable) {
            const lookup = self.symbol_table.lookupItem(right.value);
            if (lookup == null) {
                reportLiteralError(right);
                return AnalysisError.VariableOutOfScope;
            }
        }

        // currently just check the expression, nothing else
        // TODO: check the if body

        self.symbol_table.exitScope();
    }

    fn checkRetStmt(self: *Analyzer, stmt: Stmt.RetStmt) !void {
        if (stmt.value.literal.type == .primitive) {
            
        } else if (stmt.value.literal.type == .variable) {
            const lookup = self.symbol_table.lookupItem(stmt.value.literal.value);
            if (lookup == null) {
                reportLiteralError(stmt.value.literal);
                return AnalysisError.VariableOutOfScope;
            }
        }
    }

    fn reportLiteralError(lit: Expr.Literal) void {
        print("{d}:{d} : {s} | ", .{lit.span.line, lit.span.start_col, lit.value});
    }
};
