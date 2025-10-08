const std = @import("std");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Token = @import("Tokenizer.zig").Token;
const Parser = @import("Parser.zig").Parser;
const CodeGen = @import("CodeGen.zig").CodeGen;

const print = std.debug.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var tokens_list: std.array_list.Aligned(Token, null) = .empty;
    
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
        
        var tokenizer = try Tokenizer.init(allocator, line);
        defer tokenizer.deinit();
        
        var line_tokens = try tokenizer.getTokens(allocator);
        defer line_tokens.deinit(allocator);

        for (line_tokens.items) |token| {
            try tokens_list.append(allocator, token);
        }
    }

    var parser = Parser.init(tokens_list, allocator);
    const ast = try parser.parseTokens();

    var code_gen = CodeGen.init();
    defer code_gen.deinit();

    try code_gen.compile(ast);
}

pub fn reportError(line: i32, where: []const u8, message: []const u8) void {
    print("[line {d} ] Error {s}: {s}", .{line, where, message});
}
