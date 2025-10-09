const std = @import("std");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Token = @import("Tokenizer.zig").Token;
const Parser = @import("Parser.zig").Parser;
const CodeGen = @import("CodeGen.zig").CodeGen;

const print = std.debug.print;

const Error = error{
    NotIridiumFile,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var tokens_list: std.array_list.Aligned(Token, null) = .empty;

    var args = std.process.args();
    var file_name: []const u8 = "";

    while (true) {
        if (args.inner.count == 1) break;
        if (args.inner.index == 1) {
            file_name = args.next().?;
            break;
        }
        args.inner.index += 1;
    }

    const file_extension = ".ird";
    if (std.mem.count(u8, file_name, file_extension) != 1) return error.NotIridiumFile;

    const file_str = try std.fs.cwd().readFileAlloc(allocator, file_name, 1_000_000_000);

    var tokenizer = try Tokenizer.init(allocator, file_str);
    
    var line_tokens = try tokenizer.getTokens(allocator);

    for (line_tokens.items) |token| {
        try tokens_list.append(allocator, token);
    }
           
    var parser = Parser.init(tokens_list, allocator);
    const ast = try parser.parseProgram();

    var code_gen = CodeGen.init();
    try code_gen.compile(ast);

    tokenizer.deinit();
    line_tokens.deinit(allocator);
    parser.deinit();
    code_gen.deinit();
}

pub fn reportError(line: i32, where: []const u8, message: []const u8) void {
    print("[line {d} ] Error {s}: {s}", .{line, where, message});
}
