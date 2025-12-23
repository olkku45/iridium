const std = @import("std");
const Parser = @import("Parser.zig").Parser;
const Type = @import("Parser.zig").Type;
const Result = @import("Analyzer.zig").Analyzer.Result;
const Stmt = @import("Parser.zig").Stmt;
const Expr = @import("Parser.zig").Expr;
const c = @import("llvm.zig").c;

const print = std.debug.print;
const Allocator = std.mem.Allocator;

const CodeGenError = error{
    NotPutChar,
    EmptyLine,
    TargetMachineCreationFailed,
    EmitFailed,
    InvalidModule,
};

pub const CodeGen = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    allocator: std.mem.Allocator,
    symbols: std.StringHashMap(c.LLVMValueRef),

    pub fn init(allocator: std.mem.Allocator) CodeGen {
        const context = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext("test", context);
        const builder = c.LLVMCreateBuilderInContext(context);
        const symbols = std.StringHashMap(c.LLVMValueRef).init(allocator);

        return .{
            .context = context,
            .module = module,
            .builder = builder,
            .allocator = allocator,
            .symbols = symbols,
        };
    }

    pub fn deinit(self: *CodeGen) void {
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn compile(self: *CodeGen, ast: []Stmt) !void {
        try generateProgramCode(self, ast);

        c.LLVMInitializeAllTargetInfos();
        c.LLVMInitializeAllTargets();
        c.LLVMInitializeAllTargetMCs();
        c.LLVMInitializeAllAsmParsers();
        c.LLVMInitializeAllAsmPrinters();

        const target_triple = c.LLVMGetDefaultTargetTriple();
        defer c.LLVMDisposeMessage(target_triple);

        var target: c.LLVMTargetRef = undefined;
        var target_error_msg: [*c]u8 = undefined;
        if (c.LLVMGetTargetFromTriple(target_triple, &target, &target_error_msg) != 0) {
            print("Target error: {s}\n", .{target_error_msg});
            c.LLVMDisposeMessage(target_error_msg);
        }

        const cpu = "generic";
        const features = "";
        const opt_level = c.LLVMCodeGenLevelDefault;
        const reloc_mode = c.LLVMRelocPIC;
        const code_model = c.LLVMCodeModelDefault;

        const target_machine = c.LLVMCreateTargetMachine(
            target,
            target_triple,
            cpu,
            features,
            opt_level,
            reloc_mode,
            code_model,
        );

        if (target_machine == null) return error.TargetMachineCreationFailed;

        // test if module works or not
        var err_msg: [*c]u8 = null;
        if (c.LLVMVerifyModule(self.module, c.LLVMPrintMessageAction, &err_msg) != 0) {
            std.debug.print("Module verification failed: {s}\n", .{err_msg});
            c.LLVMDisposeMessage(err_msg);
            return error.InvalidModule;
        }
        
        if (c.LLVMTargetMachineEmitToFile(
            target_machine,
            self.module,
            "test.o",
            c.LLVMObjectFile,
            &target_error_msg,
        ) != 0) {
                print("Error emitting object file: {s}\n", .{target_error_msg});
                c.LLVMDisposeMessage(target_error_msg);
                return error.EmitFailed;
            }

        c.LLVMDisposeTargetMachine(target_machine);

        c.LLVMDumpModule(self.module);
    }

    fn generateProgramCode(self: *CodeGen, ast: []Stmt) !void {
        for (ast) |item| {
            switch (item) {
                .extern_fn_decl => {
                    declarePutchar(self);
                },
                .fn_decl => |declaration| {
                    try createFunction(self, declaration.name.literal.value, item);  
                },
                else => {},
            }
        }
    }

    fn createPutcharCall(self: *CodeGen, char: u8) void {
        const putchar_func = c.LLVMGetNamedFunction(self.module, "putchar");
        const putchar_type = c.LLVMGlobalGetValueType(putchar_func);

        const char_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), char, 0);
        var args = [_]c.LLVMValueRef{char_val};

        _ = c.LLVMBuildCall2(
            self.builder,
            putchar_type,
            putchar_func,
            &args,
            1,
            ""
        );
    }

    fn declarePutchar(self: *CodeGen) void {
        var param_types = [_]c.LLVMTypeRef{ c.LLVMInt32TypeInContext(self.context) };
        const func_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), &param_types, 1, 0);
        _ = c.LLVMAddFunction(self.module, "putchar", func_type);
    }

    // create main function
    fn createFunction(self: *CodeGen, name: []const u8, decl: Stmt) !void {
        // string to memory, arbitrary 200 char limit
        var buf: [200]u8 = undefined;
        _ = try std.fmt.bufPrintZ(&buf, "{s}", .{name});
        
        const func_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), null, 0, 0);
        const func = c.LLVMAddFunction(self.module, &buf, func_type);

        const block = c.LLVMAppendBasicBlock(func, "block");
        c.LLVMPositionBuilderAtEnd(self.builder, block);

        for (decl.fn_decl.fn_body) |stmt| {
            switch (stmt) {
                .expr_stmt => |s| {
                    // TODO make this arm work
                    const call = s.expr.func_call.*;
                    const arg_val = call.args[0].literal.value;
                    var putchar_char: u8 = undefined;

                    const func_ident = call.func_name.literal.value;

                    if (std.mem.eql(u8, func_ident, "println")) {
                        for (0..arg_val.len) |i| {
                            if (i == 0 or i == arg_val.len - 1) continue;
                            createPutcharCall(self, arg_val[i]);
                        }
                        createPutcharCall(self, '\n');
                    }

                    // TODO: remove hardcoding that char must be character, not number
                    if (arg_val.len == 3) {
                        putchar_char = arg_val[1];
                        createPutcharCall(self, putchar_char);
                    } else {
                        if (std.mem.eql(u8, arg_val[1..3], "\\n")) {
                            createPutcharCall(self, '\n');
                        }
                    }
                },
                .var_decl => |var_decl| {
                    try createVariableDecl(self, var_decl);  
                },
                .if_stmt => |if_stmt| {
                    const then_block = c.LLVMAppendBasicBlock(func, "if-then");
                    const else_block = c.LLVMAppendBasicBlock(func, "if-else");
                    const merge_block = c.LLVMAppendBasicBlock(func, "if-merge");
        
                    const evaluated = try generateExpr(self, if_stmt.condition);

                    _ = c.LLVMBuildCondBr(self.builder, evaluated, then_block, else_block);
                    c.LLVMPositionBuilderAtEnd(self.builder, then_block);
                    
                    for (if_stmt.if_body) |item| {
                        switch (item) {
                            .expr_stmt => |expr_stmt| {
                                const call = expr_stmt.expr.func_call.*;
                                const arg_val = call.args[0].literal.value;

                                // TODO make into separate function
                                if (std.mem.eql(u8, call.func_name.literal.value, "println")) {
                                    for (0..arg_val.len) |i| {
                                        if (i == 0 or i == arg_val.len - 1) continue;
                                        createPutcharCall(self, arg_val[i]);
                                    }
                                    createPutcharCall(self, '\n');
                                }
                            },
                            else => {},
                        }
                    }
                    _ = c.LLVMBuildBr(self.builder, merge_block);
                    c.LLVMPositionBuilderAtEnd(self.builder, else_block);
                    
                    _ = c.LLVMBuildBr(self.builder, merge_block);
                    c.LLVMPositionBuilderAtEnd(self.builder, merge_block);
                },
                .while_loop => |while_loop| {
                    const cond_block = c.LLVMAppendBasicBlock(func, "while-cond");
                    const body_block = c.LLVMAppendBasicBlock(func, "while-body");
                    const end_block = c.LLVMAppendBasicBlock(func, "while-end");

                    _ = c.LLVMBuildBr(self.builder, cond_block);

                    c.LLVMPositionBuilderAtEnd(self.builder, cond_block);
                    const eval_cond = try generateExpr(self, while_loop.cond);
                    _ = c.LLVMBuildCondBr(self.builder, eval_cond, body_block, end_block);

                    c.LLVMPositionBuilderAtEnd(self.builder, body_block);
                    for (while_loop.body) |item| {
                        switch (item) {
                            .expr_stmt => |expr| {
                                const call = expr.expr.func_call.*;
                                const arg_val = call.args[0].literal.value; // one arg lol

                                if (std.mem.eql(u8, call.func_name.literal.value, "println")) {
                                    for (0..arg_val.len) |i| {
                                        if (i == 0 or i == arg_val.len - 1) continue;
                                        createPutcharCall(self, arg_val[i]);
                                    }
                                    createPutcharCall(self, '\n');
                                }
                            },
                            else => {},
                        }
                    }
                    _ = c.LLVMBuildBr(self.builder, cond_block);

                    c.LLVMPositionBuilderAtEnd(self.builder, end_block);
                },
                else => {},
            }
        }

        const ret_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0);
        _ = c.LLVMBuildRet(self.builder, ret_val);
    }

    fn generateExpr(self: *CodeGen, expr: Expr) anyerror!c.LLVMValueRef {
        return switch (expr) {
            .binary => try generateBinExpr(self, expr.binary.*),
            .literal => try generateLiteralExpr(self, expr.literal),
            .unary => try generateUnaryExpr(self, expr.unary.*),
            .grouping => try generateExprGrouping(self, expr.grouping.*),
            .func_call => try generateFuncCallExpr(self, expr.func_call.*),
            else => null,
        };
    }

    fn generateFuncCallExpr(self: *CodeGen, call: Expr.CallExpr) !c.LLVMValueRef {
        const func = c.LLVMGetNamedFunction(self.module, call.func_name.literal.value.ptr);
        const func_type = c.LLVMGlobalGetValueType(func);

        var args: std.array_list.Aligned(c.LLVMValueRef, null) = .empty;
        for (call.args) |arg| {
            try args.append(self.allocator, try generateExpr(self, arg));
        }

        const args_slice = try args.toOwnedSlice(self.allocator);

        return c.LLVMBuildCall2(
            self.builder,
            func_type,
            func,
            args_slice.ptr,
            @intCast(args_slice.len),
            call.func_name.literal.value.ptr,
        );
    }

    fn generateUnaryExpr(self: *CodeGen, unary: Expr.UnaryExpr) !c.LLVMValueRef {
        const operand = try generateExpr(self, unary.operand);

        return switch (unary.op) {
            .NEG => c.LLVMBuildNeg(self.builder, operand, "neg"),
            .NOT => c.LLVMBuildNot(self.builder, operand, "not"),
        };
    }

    fn generateExprGrouping(self: *CodeGen, grouping: Expr) !c.LLVMValueRef {
        return generateExpr(self, grouping);
    }

    fn generateBinExpr(self: *CodeGen, bin: Expr.BinaryExpr) !c.LLVMValueRef {
        const left = try generateExpr(self, bin.left);
        const right = try generateExpr(self, bin.right);

        // TODO load value from variable
        return switch (bin.op) {
            .GREATER => c.LLVMBuildICmp(
                self.builder,
                c.LLVMIntSGT,
                left,
                right,
                "cmp"
            ),
            .GREATER_EQUAL => c.LLVMBuildICmp(
                self.builder,
                c.LLVMIntSGE,
                left,
                right,
                "greater-or-equal"
            ),
            .LESS => c.LLVMBuildICmp(
                self.builder,
                c.LLVMIntSLT,
                left,
                right,
                "less"
            ),
            .LESS_EQUAL => c.LLVMBuildICmp(
                self.builder,
                c.LLVMIntSLE,
                left,
                right,
                "less-or-equal"
            ),
            .ADD => c.LLVMBuildAdd(self.builder, left, right, "add"),
            .SUB => c.LLVMBuildSub(self.builder, left, right, "sub"),
            .MUL => c.LLVMBuildMul(self.builder, left, right, "mul"),
            .DIV => c.LLVMBuildSDiv(self.builder, left, right, "div"),
            .MODULUS => c.LLVMBuildSRem(self.builder, left, right, "mod"),
            .EQUAL_EQUAL => c.LLVMBuildICmp(
                self.builder,
                c.LLVMIntEQ,
                left,
                right,
                "equal"
            ),
            .NOT_EQUAL => c.LLVMBuildICmp(
                self.builder,
                c.LLVMIntEQ,
                left,
                right,
                "not-equal"
            ),
            .EQUAL => c.LLVMBuildStore(self.builder, right, left),
            .ADD_EQUAL => c.LLVMBuildAdd(self.builder, left, right, "add-equal"),
            .SUB_EQUAL => c.LLVMBuildSub(self.builder, left, right, "sub-equal"),
            .MUL_EQUAL => c.LLVMBuildMul(self.builder, left, right, "mul-equal"),
            .DIV_EQUAL => c.LLVMBuildSDiv(self.builder, left, right, "div-equal"),
            .AND => c.LLVMBuildAnd(self.builder, left, right, "and"),
            .OR => c.LLVMBuildOr(self.builder, left, right, "or"),
            // false as dummy
            .none => c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0),
        };
    }

    fn generateLiteralExpr(self: *CodeGen, lit: Expr.Literal) !c.LLVMValueRef {
        if (lit.type == .identifier) {
            if (std.mem.eql(u8, lit.value, "true")) {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
            } else if (std.mem.eql(u8, lit.value, "false")) {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
            }

            const alloca = self.symbols.get(lit.value);

            // arbitrary limit
            var buf: [200]u8 = undefined;
            _ = try std.fmt.bufPrintZ(&buf, "{s}", .{lit.value});
            return c.LLVMBuildLoad2(
                self.builder,
                c.LLVMInt32TypeInContext(self.context),
                alloca.?,
                &buf
            );
        }

        const val = try std.fmt.parseInt(c_ulonglong, lit.value, 10);
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), val, 0);
    }
    
    fn createVariableDecl(self: *CodeGen, decl: Stmt.VariableDecl) !void {
        const name = decl.name.literal.value;
        const var_type = decl.var_type.named.primitive;

        var ir_type: c.LLVMTypeRef = undefined;
        switch (var_type) {
            .c_int => {
                ir_type = c.LLVMInt32TypeInContext(self.context);
            },
            else => {},
        }

        // LLVM requires a null-terminated string
        var buf: [200]u8 = undefined;
        _ = try std.fmt.bufPrintZ(&buf, "{s}", .{name});

        const alloca = c.LLVMBuildAlloca(self.builder, ir_type, &buf);
        const val = try generateExpr(self, decl.value);

        _ = c.LLVMBuildStore(self.builder, val, alloca);
        
        try self.symbols.put(name, alloca);
    }
};
