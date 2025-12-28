const std = @import("std");

test "end-to-end: success cases" {
    try runTestsInDir("src/tests/end-to-end/should-succeed", .expect_success);
}

test "end-to-end: failure cases" {
    try runTestsInDir("src/tests/end-to-end/should-fail", .expect_failure);
}

fn runTestsInDir(dir_path: []const u8, mode: enum { expect_success, expect_failure }) !void {
    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".ird")) continue;

        try runSingleTest(dir, entry.name, mode);
    }
}

fn runSingleTest(dir: std.fs.Dir, filename: []const u8, mode: anytype) !void {
    const alloc = std.testing.allocator;

    const compiler_path = try std.fs.cwd().realpathAlloc(alloc, "zig-out/bin/iridium");
    defer alloc.free(compiler_path);

    const source = try dir.readFileAlloc(alloc, filename, 1024 * 1024);
    defer alloc.free(source);

    const expected = try parseExpected(alloc, source);
    defer if (expected) |e| alloc.free(e);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const source_path = try std.fmt.allocPrint(alloc, "test.ird", .{});
    defer alloc.free(source_path);

    try tmp.dir.writeFile(.{
        .sub_path = source_path,
        .data = source,
    });

    const compile_result = try std.process.Child.run(.{
        .allocator = alloc,
        .argv = &[_][]const u8{ compiler_path, source_path },
        .cwd_dir = tmp.dir,
    });
    defer alloc.free(compile_result.stdout);
    defer alloc.free(compile_result.stderr);

    if (mode == .expect_failure) {
        try std.testing.expect(compile_result.term.Exited != 0);
        if (expected) |exp| {
            try std.testing.expect(std.mem.indexOf(u8, compile_result.stderr, exp) != null);
        }
        return;
    }

    std.debug.print("comp stdout: {s}\n", .{compile_result.stdout});
    std.debug.print("comp stderr: {s}\n", .{compile_result.stderr});

    const test_o_exists = blk: {
        tmp.dir.access("test.o", .{}) catch break :blk false;
        break :blk true;
    };
    std.debug.print("test.o exists: {}\n", .{test_o_exists});

    const link_result = try std.process.Child.run(.{
        .allocator = alloc,
        .argv = &[_][]const u8{ "gcc", "test.o" },
        .cwd_dir = tmp.dir,
    });
    defer alloc.free(link_result.stdout);
    defer alloc.free(link_result.stderr);
    try std.testing.expectEqual(@as(u8, 0), link_result.term.Exited);

    const run_result = try std.process.Child.run(.{
        .allocator = alloc,
        .argv = &[_][]const u8{ "./a.out" },
        .cwd_dir = tmp.dir,
    });
    defer alloc.free(run_result.stdout);
    defer alloc.free(run_result.stderr);

    if (expected) |exp| {
        try std.testing.expectEqualStrings(exp, std.mem.trim(u8, run_result.stdout, " \n\r\t"));
    }
}

fn parseExpected(alloc: std.mem.Allocator, source: []const u8) !?[]const u8 {
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (std.mem.startsWith(u8, trimmed, "// expected_output:")) {
            const output = std.mem.trim(u8, trimmed[19..], " \t");
            return try alloc.dupe(u8, output);
        }
        if (std.mem.startsWith(u8, trimmed, "// expected_error:")) {
            const err = std.mem.trim(u8, trimmed[18..], " \t");
            return try alloc.dupe(u8, err);
        }
    }
    return null;
}
