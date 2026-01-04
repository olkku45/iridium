const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Stmt = @import("Parser.zig").Stmt;
const Expr = @import("Parser.zig").Expr;
const TopLevelStmt = @import("Parser.zig").TopLevelStmt;

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

    pub fn printAst(self: *AstPrinter, ast: []TopLevelStmt) !void {
        for (ast) |top_stmt| {
            try printTopStmt(self, top_stmt);
        }
    }

    fn printTopStmt(self: *AstPrinter, top_stmt: TopLevelStmt) !void {
        switch (top_stmt) {
            .extern_fn_decl => try printExternDecl(self, top_stmt),
            .fn_decl => try printFnDecl(self, top_stmt),
        }
    }

    fn printStmt(self: *AstPrinter, stmt: Stmt) anyerror!void {
        switch (stmt) {
            .if_stmt => try printIfStmt(self, stmt),
            .var_decl => try printVarDecl(self, stmt),
            .expr_stmt => try printExprStmt(self, stmt),
            .ret_stmt => try printRetStmt(self, stmt),
            .while_loop => try printWhileLoop(self, stmt),
            .error_node => try printError(self),
        }
    }

    fn printExpr(self: *AstPrinter, expr: Expr) anyerror!void {
        switch (expr) {
            .literal => try printLiteralExpr(self, expr),
            .binary => try printBinaryExpr(self, expr),
            .unary => try printUnaryExpr(self, expr),
            .grouping => try printExprGrouping(self, expr),
            .func_call => try printCallExpr(self, expr),
            else => {},
        }
    }

    fn writeIndent(self: *AstPrinter) !void {
        var i: u32 = 0;
        // cool syntax!
        while (i < self.indent_level * self.indent_size) : (i += 1) {
            try self.writer.writeByte(' ');
        }
    }

    fn printCallExpr(self: *AstPrinter, expr: Expr) !void {
        try self.writeIndent();
        try self.writer.writeAll("Call expression\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Function name\n");

        self.indent();

        try self.printExpr(expr.func_call.*.func_name);

        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Function arguments\n");

        self.indent();

        for (expr.func_call.*.args) |arg| {
            try self.printExpr(arg);
        }

        self.dedent();
        self.dedent();
    }

    fn printExprGrouping(self: *AstPrinter, expr: Expr) !void {
        try self.writeIndent();
        try self.writer.writeAll("Expression grouping\n");

        self.indent();
        try printExpr(self, expr.grouping.*);
        self.dedent();
    }

    fn printUnaryExpr(self: *AstPrinter, expr: Expr) !void {
        try self.writeIndent();
        try self.writer.writeAll("Unary expression\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Operator\n");

        try self.writeIndent();
        try self.writer.writeAll("Value\n");

        self.indent();
        try self.printExpr(expr.unary.*.operand);
        self.dedent();
        
        self.dedent();
    }

    fn printBinaryExpr(self: *AstPrinter, expr: Expr) !void {
        try self.writeIndent();
        try self.writer.writeAll("Binary Expresssion\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Left\n");

        self.indent();
        try self.printExpr(expr.binary.*.left);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Operator\n");

        try self.writeIndent();
        try self.writer.writeAll("Right\n");

        self.indent();
        try self.printExpr(expr.binary.*.right);
        self.dedent();
        
        self.dedent();
    }

    fn printLiteralExpr(self: *AstPrinter, expr: Expr) !void {
        try self.writeIndent();
        try self.writer.writeAll("Literal Expression\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll(expr.literal.value);
        try self.writer.writeByte('\n');

        self.dedent();
    }

    fn printWhileLoop(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("While\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Condition:\n");

        self.indent();
        try self.printExpr(stmt.while_loop.cond);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Body:\n");

        self.indent();

        for (stmt.while_loop.body) |item| {
            try self.printStmt(item);
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
        try self.writer.writeAll("If statement\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Condition:\n");

        self.indent();
        try self.printExpr(stmt.if_stmt.condition);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("If Body:\n");

        self.indent();

        for (stmt.if_stmt.if_body) |item| {
            try self.printStmt(item);
        }

        self.dedent();
        self.dedent();
    }

    fn printExternDecl(self: *AstPrinter, stmt: TopLevelStmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("Extern Fn\n");

        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Name\n");

        self.indent();
        try self.printExpr(stmt.extern_fn_decl.name);
        self.dedent();
        
        self.dedent();
    }

    fn printFnDecl(self: *AstPrinter, stmt: TopLevelStmt) anyerror!void {
        try self.writeIndent();
        try self.writer.writeAll("Function name\n");

        self.indent();
        try self.printExpr(stmt.fn_decl.name);
        self.dedent();

        // no parameters yet...

        try self.writeIndent();
        try self.writer.writeAll("Function Body:\n");

        self.indent();

        for (stmt.fn_decl.body) |item| {
            try self.printStmt(item);
        }

        self.dedent();
    }

    fn printVarDecl(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("Variable declaration\n");
        
        self.indent();

        try self.writeIndent();
        try self.writer.writeAll("Name\n");

        self.indent();
        try self.printExpr(stmt.var_decl.name);
        self.dedent();

        try self.writeIndent();
        try self.writer.writeAll("Value\n");

        self.indent();
        try self.printExpr(stmt.var_decl.value);
        self.dedent();

        self.dedent();
    }

    fn printExprStmt(self: *AstPrinter, stmt: Stmt) !void {
        try printExpr(self, stmt.expr_stmt.expr);
    }

    fn printRetStmt(self: *AstPrinter, stmt: Stmt) !void {
        try self.writeIndent();
        try self.writer.writeAll("Return value\n");

        self.indent();
        try self.printExpr(stmt.ret_stmt.value);
        self.dedent();
    }

    fn indent(self: *AstPrinter) void {
        self.indent_level += 1;
    }

    fn dedent(self: *AstPrinter) void {
        self.indent_level -= 1;
    }
};
