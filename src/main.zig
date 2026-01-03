const std = @import("std");
const Tokenizer = @import("Tokenizer.zig").Tokenizer;
const Token = @import("Tokenizer.zig").Token;
const Parser = @import("Parser.zig").Parser;
const Analyzer = @import("Analyzer.zig").Analyzer;
//const CodeGen = @import("CodeGen.zig").CodeGen;
const AstPrinter = @import("AstPrinter.zig").AstPrinter;
const IRGenerator = @import("IRGenerator.zig").IRGenerator;
const IRPrinter = @import("IRPrinter.zig").IRPrinter;

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

test {
    print("Tokenizer tests...\n", .{});
    _ = @import("Tokenizer.zig");
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var args = std.process.args();

    _ = args.skip();
    const file_name = args.next() orelse return error.SomethingWentWrong;

    if (!std.mem.endsWith(u8, file_name, ".ird")) return error.NotIridiumFile;

    const file_str = try std.fs.cwd().readFileAlloc(allocator, file_name, 1_000_000_000);

    var tokenizer = try Tokenizer.init(allocator, file_str);
    const line_tokens = try tokenizer.getTokens();

    //for (0..line_tokens.len) |i| {
    //    print("{d} : {any}\n", .{i, line_tokens[i]});
    //}

    var parser = Parser.init(line_tokens, allocator);
    const ast = try parser.parseTokens();

    // check parsing errors before going to semantic analysis
    const diagnostics = try parser.getDiagnostics();
    if (diagnostics.len > 0) {
        for (diagnostics) |diag| {
            switch (diag.severity) {
                .err => {
                    if (diag.msg == null) {
                        print(
                            "Error at line {d}: after '{s}'\n",
                            .{diag.token.span.?.line, diag.token.lexeme}
                        );
                    } else print(
                        "Error at line {d}: {s} after '{s}'\n",
                        .{diag.token.span.?.line, diag.msg.?, diag.token.lexeme}
                    );
                },
                else => {},
            }
        }
        std.process.exit(0);
    }
    
    const stdout_buffer = try allocator.alloc(u8, 8_000);
    var stdout_writer = std.fs.File.stdout().writer(stdout_buffer);
    const stdout = &stdout_writer.interface;
    
    var printer = AstPrinter.init(stdout);
    try printer.printAst(ast);

    try stdout.flush();

    var analyzer = try Analyzer.init(ast, allocator);
    _ = try analyzer.analyze();

    if (analyzer.diagnostics.items.len > 0) {
        for (analyzer.diagnostics.items) |diag| {
            switch (diag.severity) {
                .err => {
                    if (diag.msg == null) {
                        print(
                            "Error at line {d}: from '{s}'\n",
                            .{diag.span.line, diag.value}
                        );
                    } else print(
                        "Error at line {d}: {s} from '{s}'\n",
                        .{diag.span.line, diag.msg.?, diag.value}
                    );
                },
                .warn => {
                    if (diag.msg != null) {
                        print(
                            "Warning at line {d}: {s} from '{s}'\n",
                            .{diag.span.line, diag.msg.?, diag.value}
                        );
                    }
                },
                .info => {},
            }
        }
        std.process.exit(0);
    }

    var ir_gen = IRGenerator.init(ast, allocator);
    const instructions = try ir_gen.generateIr();

    var ir_printer = IRPrinter.init(instructions, allocator, stdout);
    try ir_printer.printInstructions();

    try stdout.flush();

    // TODO add codegen
}
