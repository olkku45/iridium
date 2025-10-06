const std = @import("std");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Parser = @import("Parser.zig").Parser;
const CodeGen = @import("CodeGen.zig").CodeGen;

const print = std.debug.print;

pub fn main() !void {
    while (true) {
        var stdin_buffer: [1024]u8 = undefined;
        var stdout_buffer: [1024]u8 = undefined;

        var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);

        const stdin = &stdin_reader.interface;
        const stdout = &stdout_writer.interface;

        try stdout.writeAll("> ");
        try stdout.flush();

        const line = try stdin.takeDelimiterExclusive('\n');

        if (line.len == 0) break;

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allocator = gpa.allocator();
        
        var tokenizer = try Tokenizer.init(allocator, line);
        defer tokenizer.deinit();
        
        var tokens = try tokenizer.getTokens(allocator);
        defer tokens.deinit(allocator);
        
        //for (0..tokens.items.len) |i| {
            //try stdout.print("{}\n", .{tokens.items[i]});
            //try stdout.flush();
        //}

        var parser = Parser.init(tokens, allocator);
        const ast = try parser.parseTokens();

        //try stdout.print("{}\n", .{ast});
        //try stdout.flush();

        CodeGen.generateCode(ast);
    }
}

pub fn reportError(line: i32, where: []const u8, message: []const u8) void {
    print("[line {d} ] Error {s}: {s}", .{line, where, message});
}
