const std = @import("std");
const Instruction = @import("IRGenerator.zig").Instruction;

fn createAsmFile() !std.fs.File {
    const file = try std.fs.cwd().createFile("test.s", .{}); // assuming unix
    return file;
}

pub const CodeGen = struct {
    allocator: std.mem.Allocator,
    instructions: []Instruction,
    file: std.fs.File,

    pub fn init(instructions: []Instruction, allocator: std.mem.Allocator) CodeGen {
        return CodeGen{
            .instructions = instructions,
            .allocator = allocator,
            .file = createAsmFile(),
        };
    }

    pub fn deinit(self: *CodeGen) void {
        self.file.close();
    }

    pub fn writeAsm(self: *CodeGen) !void {
        for (self.instructions) |instruction| {
            switch (instruction) {
                .function => {},
                .allocation => {},
                .expression => {},
                .ret => {},
                .assignment => {},
                .func_end => {},
            }
        }
    }
};
