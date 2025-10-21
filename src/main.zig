const std = @import("std");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Token = @import("Tokenizer.zig").Token;
const Parser = @import("Parser.zig").Parser;
const Analyzer = @import("Analyzer.zig").Analyzer;
const CodeGen = @import("CodeGen.zig").CodeGen;
const AstPrinter = @import("AstPrinter.zig").AstPrinter;

const print = std.debug.print;

const Error = error{
    NotIridiumFile,
};

pub const Span = struct {
    line: usize,
    start_col: usize,
    end_col: usize,
    source_file: ?[]const u8,  
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    //var tokens_list: std.array_list.Aligned(Token, null) = .empty;

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
    const line_tokens = try tokenizer.getTokens(allocator);

    //for (0..line_tokens.len) |i| {
    //    print("{d} : {any}\n\n", .{i, line_tokens[i]});
    //}

    var parser = Parser.init(line_tokens, allocator);
    const ast = try parser.parseTokens();

    var stdout_buffer: [100_000]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);

    const stdout = &stdout_writer.interface;
    
    var printer = AstPrinter.init(stdout);
    try printer.printAst(ast);

    try stdout.flush();

    var analyzer = try Analyzer.init(ast, allocator);
    const analyzed = try analyzer.analyzeAst();

    var code_gen = CodeGen.init(allocator);
    try code_gen.compile(analyzed);

    code_gen.deinit();
}

pub fn reportError(line: usize, where: []const u8, message: []const u8) void {
    print("[line {d} ] Error {s}: {s}", .{line, where, message});
}
