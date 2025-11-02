const std = @import("std");
const Stmt = @import("Parser.zig").Stmt;

pub const TypeChecker = struct {
    ast: []Stmt,
    allocator: std.mem.Allocator,
};
