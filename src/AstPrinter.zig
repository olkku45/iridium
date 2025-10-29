const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Stmt = @import("Parser.zig").Stmt;
const Expr = @import("Parser.zig").Expr;

pub const AstPrinter = struct {
    writer: *std.io.Writer,
    indent_level: u32,
    indent_size: u32,

    pub fn init(writer: *std.io.Writer) AstPrinter {
        return .{
            .writer = writer,
            .indent_level = 0,
            .indent_size = 2,
        };
    }

    pub fn printAst(self: *AstPrinter, ast: []Stmt) !void {
        for (ast) |stmt| {
            try printNode(self, stmt);
        }
    }

    fn printNode(self: *AstPrinter, stmt: Stmt) anyerror!void {
        switch (stmt) {
            .if_stmt => try printIfStmt(self, stmt),
            .extern_fn_decl => try printExternDecl(self, stmt),
            .fn_decl => try printFnDecl(self, stmt),
            .var_decl => try printVarDecl(self, stmt),
            .expr_stmt => try printExprStmt(self, stmt),
            .ret_stmt => try printRetStmt(self, stmt),
            .while_loop => try printWhileLoop(self, stmt),
            .error_node => try printError(self),
        }
    }

    fn writeIndent(self: *AstPrinter) !void {
        var i: u32 = 0;
        // cool syntax!
        while (i < self.indent_level * self.indent_size) : (i += 1) {
            try self.writer.writeByte(' ');
        }
    }

    fn printWhileLoop(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("While\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Condition:\n");
        self.indent();
        try self.writer.writeAll(stmt.while_loop.cond.literal.value);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Body:\n");
        self.indent();

        for (stmt.while_loop.body) |item| {
            try self.printNode(item);
        }

        self.dedent();
        self.dedent();
    }

    fn printError(self: *AstPrinter) !void {
        try self.writeIndent();
        try self.writer.writeAll("ERROR\n");
    }

    fn printIfStmt(self: *AstPrinter, stmt: Stmt) anyerror!void {
        try self.writeIndent();
        try self.writer.writeAll("If\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Condition:\n");
        self.indent();
        try self.printBinaryExpr(stmt.if_stmt.condition.binary.*);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("If Body:\n");
        self.indent();

        for (stmt.if_stmt.if_body) |item| {
            try self.printNode(item);
        }

        self.dedent();
        self.dedent();
    }

    fn printExternDecl(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("Extern Fn\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Name\n");
        self.indent();
        try self.writer.writeAll(stmt.extern_fn_decl.name.literal.value);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Arg Type\n");
        self.indent();
        // cannot print type currently
        //try self.writer.writeAll(stmt.extern_fn_decl.arg_type.named.primitive);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Return Type\n");
        self.indent();
        // cannot print type currently
        //try self.writer.writeAll(stmt.extern_fn_decl.ret_type.named.primitive);
        self.dedent();
    }

    fn printFnDecl(self: *AstPrinter, stmt: Stmt) anyerror!void {
        try self.writeIndent();
        try self.writer.print("Function Declaration: {s}\n", .{stmt.fn_decl.name.literal.value});

        self.indent();

        // no parameters yet...

        try self.writeIndent();
        try self.writer.writeAll("Returns: ");
        // cannot print type currently
        //try self.writer.writeAll(stmt.fn_decl.ret_type.named.primitive);
        try self.writer.writeByte('\n');

        try self.writeIndent();
        try self.writer.writeAll("Function Body:\n");
        self.indent();

        for (stmt.fn_decl.fn_body) |item| {
            try self.printNode(item);
        }

        self.dedent();
        self.dedent();
    }

    fn printVarDecl(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.print("Variable Declaration: {s}", .{stmt.var_decl.name.literal.value});
        try self.writer.writeByte('\n');

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Variable type: ");
        self.indent();
        // cannot print type currently
        //try self.writer.writeAll(stmt.var_decl.var_type.primitive);
        try self.writer.writeByte('\n');
        self.dedent();

        try self.writeIndent();
        try self.writer.print("Value: {s}\n", .{stmt.var_decl.value.literal.value});

        self.dedent();
    }

    fn printExprStmt(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("Expression\n");

        self.indent();

        switch (stmt.expr_stmt.expr) {
            .binary => |expr| try printBinaryExpr(self, expr.*),
            .func_call => |call| try printCallExpr(self, call.*),
            else => {},
        }

        self.dedent();
    }

    fn printRetStmt(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("Return value: ");
        try self.writer.writeAll(stmt.ret_stmt.value.literal.value);
        try self.writer.writeByte('\n');
    }

    fn printCallExpr(self: *AstPrinter, expr: Expr.CallExpr) !void {
        try self.writeIndent();
        try self.writer.print("Function call: {s}\n", .{expr.func_name.literal.value});

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Arguments: \n");

        self.indent();
        
        try self.writeIndent();
        try self.writer.writeAll(expr.args.literal.value);
        try self.writer.writeByte('\n');

        self.dedent();
        self.dedent();
    }

    fn printBinaryExpr(self: *AstPrinter, expr: Expr.BinaryExpr) !void {
        try self.writeIndent();
        try self.writer.writeAll("Binary Expression\n");

        self.indent();

        try self.writeIndent();
        try self.writer.print("Left: {s}\n", .{expr.left.literal.value});

        try self.writeIndent();
        try self.writer.print("Operator: {any}\n", .{expr.op});

        try self.writeIndent();
        try self.writer.print("Right: {s}\n", .{expr.right.literal.value});

        self.dedent();
    }

    fn indent(self: *AstPrinter) void {
        self.indent_level += 1;
    }

    fn dedent(self: *AstPrinter) void {
        self.indent_level -= 1;
    }
};
