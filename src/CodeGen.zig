const std = @import("std");
const Parser = @import("Parser.zig").Parser;
const Type = @import("Parser.zig").Type;
const Result = @import("Analyzer.zig").Analyzer.Result;
const Stmt = @import("Parser.zig").Stmt;
const c = @import("llvm.zig").c;

const print = std.debug.print;
const Allocator = std.mem.Allocator;

const CodeGenError = error{
    NotPutChar,
    EmptyLine,
    TargetMachineCreationFailed,
    EmitFailed,
};

pub const CodeGen = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CodeGen {
        const context = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext("putchar test", context);
        const builder = c.LLVMCreateBuilderInContext(context);

        return .{
            .context = context,
            .module = module,
            .builder = builder,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CodeGen) void {
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn compile(self: *CodeGen, res: Result) !void {
        try generateProgramCode(self, res);

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

    fn generateProgramCode(self: *CodeGen, analyzed: Result) !void {
        const ast = analyzed.ast;
    
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

        const char_val = c.LLVMConstInt(c.LLVMInt32Type(), char, 0);
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
        var param_types = [_]c.LLVMTypeRef{ c.LLVMInt32Type() };
        const func_type = c.LLVMFunctionType(c.LLVMInt32Type(), &param_types, 1, 0);
        _ = c.LLVMAddFunction(self.module, "putchar", func_type);
    }

    // create main function
    fn createFunction(self: *CodeGen, name: []const u8, decl: Stmt) !void {
        // string to memory
        var buf: [200]u8 = undefined;
        const name_copy = try std.fmt.bufPrintZ(&buf, "{s}", .{name});
        
        const func_type = c.LLVMFunctionType(c.LLVMInt32Type(), null, 0, 0);
        const func = c.LLVMAddFunction(self.module, name_copy.ptr, func_type);

        const block = c.LLVMAppendBasicBlock(func, "block");
        c.LLVMPositionBuilderAtEnd(self.builder, block);

        for (decl.fn_decl.fn_body) |stmt| {
            switch (stmt) {
                .expr_stmt => |s| {
                    const call = s.expr.func_call.*;
                    const arg = call.args[0];
                    var putchar_char: u8 = undefined;

                    const func_ident = call.func_name.literal.value;

                    if (std.mem.eql(u8, func_ident, "println")) {
                        for (0..arg.literal.value.len) |i| {
                            if (i == 0 or i == arg.literal.value.len - 1) continue;
                            createPutcharCall(self, arg.literal.value[i]);
                        }
                        createPutcharCall(self, '\n');
                    }

                    // TODO: remove hardcoding that char must be character, not number
                    if (arg.literal.value.len == 3) {
                        putchar_char = arg.literal.value[1];
                        createPutcharCall(self, putchar_char);
                    } else {
                        if (std.mem.eql(u8, arg.literal.value[1..3], "\\n")) {
                            createPutcharCall(self, '\n');
                        }
                    }
                },
                .var_decl => |var_decl| {
                    try createVariableDecl(self, var_decl);  
                },
                else => {},
            }
        }

        const ret_val = c.LLVMConstInt(c.LLVMInt32Type(), 0, 0);
        _ = c.LLVMBuildRet(self.builder, ret_val);
    }

    fn createVariableDecl(self: *CodeGen, decl: Stmt.VariableDecl) !void {
        const name = decl.name.literal.value;
        const var_type = decl.var_type.named.primitive;

        var ir_type: c.LLVMTypeRef = undefined;
        switch (var_type) {
            .c_int => {
                ir_type = c.LLVMInt32Type();
            },
            else => {},
        }

        // LLVM requires a null-terminated string
        var buf: [200]u8 = undefined;
        _ = try std.fmt.bufPrintZ(&buf, "{s}", .{name});

        _ = c.LLVMBuildAlloca(self.builder, ir_type, &buf);
    }
};
