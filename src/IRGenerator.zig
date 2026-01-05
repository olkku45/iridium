const std = @import("std");
const Stmt = @import("Parser.zig").Stmt;
const Expr = @import("Parser.zig").Expr;
const Type = @import("Parser.zig").Type;
const BinaryOp = @import("Parser.zig").BinaryOp;
const UnaryOp = @import("Parser.zig").UnaryOp;
const TopLevelStmt = @import("Parser.zig").TopLevelStmt;

const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Parser = @import("Parser.zig").Parser;

const Parameter = struct {
    name: Expr.Literal,
    param_type: Type,
};

pub const Instruction = union(enum) {
    function: Function,
    allocation: Allocation,
    expression: Expression,
    ret: FuncReturn,
    assignment: Assignment,
    func_end: FuncEnd,
    
    pub const Function = struct {
        name: Expr.Literal,
        func_type: Type,
        params: ?[]Parameter,
    };

    pub const Allocation = struct {
        name: Expr.Literal,
        alloc_type: Type,
    };

    pub const Expression = struct {
        temp: []const u8,
        op: union(enum) { bin: BinaryOp, un: UnaryOp },
        first: []const u8,
        second: ?[]const u8,
    };

    pub const FuncReturn = struct {
        ret_val: Expr,
    };

    pub const Assignment = struct {
        to: []const u8,
        from: []const u8, // temp e.g. t0
    };

    pub const FuncEnd = struct {};
};

fn initInstructions() std.ArrayList(Instruction) {
    const arr: std.array_list.Aligned(Instruction, null) = .empty;
    return arr;
}

pub const IRGenerator = struct {
    ast: []TopLevelStmt,
    ir: std.ArrayList(Instruction),
    allocator: std.mem.Allocator,
    temp_count: usize,

    pub fn init(ast: []TopLevelStmt, allocator: std.mem.Allocator) IRGenerator {
        return IRGenerator{
            .ast = ast,
            .ir = initInstructions(),
            .allocator = allocator,
            .temp_count = 0,
        };
    }

    pub fn generateIr(self: *IRGenerator) ![]Instruction {
        for (self.ast) |stmt| {
            try generateTopLevelInstruction(self, stmt);
        }
        const slice = try self.ir.toOwnedSlice(self.allocator);
        return slice;
    }

    fn generateTopLevelInstruction(self: *IRGenerator, stmt: TopLevelStmt) anyerror!void {
        switch (stmt) {
            .fn_decl => |fn_decl| {
                try generateFunctionInstruction(self, fn_decl);
            },
            else => {},
        }
    }

    fn generateInstruction(self: *IRGenerator, stmt: Stmt) anyerror!void {
        switch (stmt) {
            .var_decl => |var_decl| {
                try generateVarInstruction(self, var_decl);
            },
            .ret_stmt => |ret| {
                try generateRetInstruction(self, ret);
            },
            else => {},
        }
    } 

    fn generateFunctionInstruction(self: *IRGenerator, fn_decl: TopLevelStmt.FnDecl) !void {
        const func = Instruction{ .function = .{
            .func_type = fn_decl.ret_type,
            .name = fn_decl.name.literal,
            .params = null, // no params yet
        }};
        try self.ir.append(self.allocator, func);

        for (fn_decl.body) |stmt| {
            try generateInstruction(self, stmt);
        }

        const end = Instruction{ .func_end = Instruction.FuncEnd{} };
        try self.ir.append(self.allocator, end);
    }

    fn generateVarInstruction(self: *IRGenerator, var_decl: Stmt.VariableDecl) !void {
        const variable = Instruction{ .allocation = .{
            .alloc_type = var_decl.var_type,
            .name = var_decl.name.literal,
        }};
        try self.ir.append(self.allocator, variable);

        const temp = try generateExpr(self, var_decl.value);

        const assignment = Instruction{ .assignment = .{
            .to = var_decl.name.literal.value,
            .from = temp.?,
        }};
        try self.ir.append(self.allocator, assignment);
    }

    fn generateRetInstruction(self: *IRGenerator, ret_stmt: Stmt.RetStmt) !void {
        const ret = Instruction{ .ret = .{
            .ret_val = ret_stmt.value,
        }};
        try self.ir.append(self.allocator, ret);
    }

    fn generateExpr(self: *IRGenerator, expr: Expr) !?[]const u8 {
        switch (expr) {
            .literal => |lit| {
                return lit.value;
            },
            .binary => |bin| {
                const left = try generateExpr(self, bin.left);
                const right = try generateExpr(self, bin.right);

                const temp = try createTemp(self);

                const instruction = Instruction{ .expression = .{
                    .temp = temp,
                    .op = .{ .bin = bin.op },
                    .first = left.?,
                    .second = right.?,
                }};
                
                try self.ir.append(self.allocator, instruction);

                return temp;
            },
            .unary => |un| {
                const operand = try generateExpr(self, un.operand);

                const temp = try createTemp(self);

                const instruction = Instruction{ .expression = .{
                    .temp = temp,
                    .op = .{ .un = un.op },
                    .first = operand.?,
                    .second = null,
                }};

                try self.ir.append(self.allocator, instruction);

                return temp;
            },
            .grouping => |group| {
                // value not needed
                const temp = try generateExpr(self, group.*);
                return temp;
            },
            .func_call => {},
            else => {},
        }
        return null;
    }

    fn createTemp(self: *IRGenerator) ![]const u8 {
        const temp = try std.fmt.allocPrint(self.allocator, "t{d}", .{self.temp_count});
        self.temp_count += 1;
        return temp;
    }
};

// ====================================
// TESTS
// ====================================

const testing = std.testing;

fn testGenerateIr(allocator: std.mem.Allocator, source: []const u8) ![]Instruction {
    var tokenizer = try Tokenizer.init(allocator, source);
    const tokens = try tokenizer.getTokens();

    var parser = Parser.init(tokens, allocator);
    const ast = try parser.parseTokens();

    var ir_gen = IRGenerator.init(ast, allocator);
    return try ir_gen.generateIr();
}

// ====================================
// IR STRUCTURE TESTS
// ====================================

test "empty function" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const ir = try testGenerateIr(allocator,
        \\fn main() => i32 {
        \\}
    );

    try testing.expect(ir[0] == .function);
    try testing.expect(ir[1] == .func_end);
    try testing.expect(ir[0].function.func_type == .INT32);
}

test "function with return" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const ir = try testGenerateIr(allocator,
        \\fn main() => i32 {
        \\    return 0;
        \\}
    );

    try testing.expect(ir[1] == .ret);
    try testing.expectEqualStrings("0", ir[1].ret.ret_val.literal.value);
}

test "variable declaration and temp expressions" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const ir = try testGenerateIr(allocator,
        \\fn main() => i32 {
        \\    let x: i32 = 3 + 2 * 1;
        \\}
    );
        
    try testing.expect(ir.len == 6);
    try testing.expect(ir[0] == .function);
    try testing.expect(ir[1] == .allocation);
    
    try testing.expect(ir[2] == .expression);
    try testing.expectEqualStrings(ir[2].expression.temp, "t0");
    try testing.expectEqualStrings(ir[2].expression.first, "2");
    try testing.expectEqualStrings(ir[2].expression.second.?, "1");
    
    try testing.expect(ir[3] == .expression);
    try testing.expectEqualStrings(ir[3].expression.temp, "t1");
    try testing.expectEqualStrings(ir[3].expression.second.?, "t0");
    
    try testing.expect(ir[4] == .assignment);
    try testing.expect(ir[5] == .func_end);
}
