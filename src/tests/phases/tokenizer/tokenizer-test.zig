const std = @import("std");
const Tokenizer = @import("../../src/Tokenizer.zig").Tokenizer;

test "tokenizer produces correct tokens" {
    const max: usize = 1_000_000;
    const file_str = try std.fs.cwd().readFileAlloc(
        std.testing.allocator,
        "code.ird",
        max
    );

    var tokenizer = try Tokenizer.init(std.testing.allocator, file_str);
    
}
