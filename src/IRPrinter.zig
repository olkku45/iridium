const std = @import("std");
const IRGenerator = @import("IRGenerator.zig").IRGenerator;
const Instruction = @import("IRGenerator.zig").Instruction;

pub const IRPrinter = struct {
    allocator: std.mem.Allocator,
    instructions: []Instruction,
    writer: *std.io.Writer,
    indent_level: u32,
    indent_size: u32,
    
    pub fn init(instructions: []Instruction, allocator: std.mem.Allocator, writer: *std.io.Writer) IRPrinter {
        return IRPrinter{
            .instructions = instructions,
            .allocator = allocator,
            .writer = writer,
            .indent_level = 0,
            .indent_size = 2,
        };
    }

    pub fn printInstructions(self: *IRPrinter) !void {
        for (self.instructions) |inst| {
            switch (inst) {
                .function => |func| {
                    try printFunctionInstruction(self, func);
                    self.indent();
                },
                .allocation => |alloc| {
                    try printAllocInstruction(self, alloc);
                },
                .expression => |expr| {
                    try printExprInstruction(self, expr);
                },
                .ret => |ret| {
                    try printRetInstruction(self, ret);
                },
                .assignment => |assign| {
                    try printAssignmentInstruction(self, assign);
                },
                .func_end => {
                    self.dedent();
                    try printFuncEnd(self);
                }
            }
        }
    }

    fn printFunctionInstruction(self: *IRPrinter, func: Instruction.Function) !void {
        try self.writeIndent();
        try self.writer.writeAll("proc ");
        try self.writer.writeAll(func.name.value);
        try self.writer.writeByte('\n');

        if (func.params == null) {
            try self.writeIndent();
            try self.writer.writeAll("param null\n");
        } else {
            // loop over params when they exist
        }

        try self.writeIndent();
        try self.writer.writeAll("type ");
        try self.writer.print("{any}\n", .{func.func_type});

        try self.writeIndent();
        try self.writer.writeAll("start\n");
    }

    fn printAllocInstruction(self: *IRPrinter, alloc: Instruction.Allocation) !void {
        try self.writeIndent();
        try self.writer.print("alloc {s} type {any}\n", .{alloc.name.value, alloc.alloc_type});
    }

    fn printExprInstruction(self: *IRPrinter, expr: Instruction.Expression) !void {
        switch (expr.op) {
            .bin => |op| {
                switch (op) {
                    .ADD => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} + {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .SUB => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} - {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .MUL => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} * {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .DIV => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} / {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .MODULUS => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} % {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .LESS => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} < {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .GREATER => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} > {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .AND => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} and {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    .OR => {
                        try self.writeIndent();
                        try self.writer.print("{s} = {s} or {s}\n", .{expr.temp, expr.first, expr.second.?});
                    },
                    else => {},
                }
            },
            .un => |op| {
                switch (op) {
                    .NOT => {
                        try self.writeIndent();
                        try self.writer.print("{s} = !{any}\n", .{expr.temp, expr.first});
                    },
                    .NEG => {
                        try self.writeIndent();
                        try self.writer.print("{s} = -{any}\n", .{expr.temp, expr.first});
                    },
                }
            },
        }
    }

    fn printRetInstruction(self: *IRPrinter, ret: Instruction.FuncReturn) !void {
        try self.writeIndent();
        try self.writer.print("ret {s}\n", .{ret.ret_val.literal.value});
    }

    fn printAssignmentInstruction(self: *IRPrinter, assignment: Instruction.Assignment) !void {
        try self.writeIndent();
        try self.writer.print("assign {s} val {s}\n", .{assignment.to, assignment.from});
    }

    fn printFuncEnd(self: *IRPrinter) !void {
        try self.writeIndent();
        try self.writer.writeAll("end\n");
    }

    fn indent(self: *IRPrinter) void {
        self.indent_level += 1;
    }

    fn dedent(self: *IRPrinter) void {
        self.indent_level -= 1;
    }
    
    fn writeIndent(self: *IRPrinter) !void {
        var i: u32 = 0;
        while (i < self.indent_level * self.indent_size) : (i += 1) {
            try self.writer.writeByte(' ');
        }
    }
};
