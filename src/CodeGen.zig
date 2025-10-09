const std = @import("std");
const Parser = @import("Parser.zig").Parser;
const Node = @import("Parser.zig").Node;

const print = std.debug.print;
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/ExecutionEngine.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
});

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

    pub fn init() CodeGen {
        const context = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext("putchar test", context);
        const builder = c.LLVMCreateBuilderInContext(context);

        return .{
            .context = context,
            .module = module,
            .builder = builder, 
        };
    }

    pub fn deinit(self: *CodeGen) void {
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn compile(self: *CodeGen, ast: Node) !void {
        declarePutchar(self);

        createMain(self, ast);

        var error_msg: [*c]u8 = null;
        if (c.LLVMPrintModuleToFile(self.module, "putchar.ll", &error_msg) != 0) {
            print("An error occured writing to file: {s}\n", .{error_msg});
            c.LLVMDisposeMessage(error_msg);
        }

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

    fn declarePutchar(self: *CodeGen) void {
        var param_types = [_]c.LLVMTypeRef{ c.LLVMInt32Type() };
        const func_type = c.LLVMFunctionType(c.LLVMInt32Type(), &param_types, 1, 0);
        _ = c.LLVMAddFunction(self.module, "putchar", func_type);
    }

    fn createMain(self: *CodeGen, ast: Node) void {
        const main_type = c.LLVMFunctionType(c.LLVMInt32Type(), null, 0, 0);
        const main = c.LLVMAddFunction(self.module, "main", main_type);

        const entry = c.LLVMAppendBasicBlock(main, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        const putchar_func = c.LLVMGetNamedFunction(self.module, "putchar");

        if (putchar_func == null) {
            print("putchar not declared. \n", .{});
            return;
        }

        const putchar_type = c.LLVMGlobalGetValueType(putchar_func);

        const program = ast.program;
        
        for (program.program_ast.items) |node_ptr| {
            var char: u8 = 0;
            
            switch (node_ptr.*) {
                .function_call => |call| {
                    const arg = call.func_argument.*;
                    char = arg.literal.value[1];
                },
                else => {},
            }

            const char_val = c.LLVMConstInt(c.LLVMInt32Type(), char, 0);
            var args = [_]c.LLVMValueRef{char_val};
            _ = c.LLVMBuildCall2(
                self.builder,
                putchar_type,
                putchar_func,
                &args,
                1,
                "",
            );
        }

        const nl = '\n';
        const nl_val = c.LLVMConstInt(c.LLVMInt32Type(), nl, 0);
        var newline = [_]c.LLVMValueRef{nl_val};

        _ = c.LLVMBuildCall2(
            self.builder,
            putchar_type,
            putchar_func,
            &newline,
            1,
            ""
        );

        const ret_val = c.LLVMConstInt(c.LLVMInt32Type(), 0, 0);

        _ = c.LLVMBuildRet(self.builder, ret_val);
    }

    fn identifier(ident: *Node) c.LLVMValueRef {
        const value = ident.identifier.name;
        return c.LLVMConstString(value, value.len, 1);
    }

    fn argument(arg: *Node) !c.LLVMValueRef {
        return try literal(arg.argument.argument_literal.*.literal.value);
    }

    // return the char in the putchar call
    fn literal(lit: []const u8) !c.LLVMValueRef {
        // TODO: tokenize/parse characters better,
        // take just the character not the quotes?
        const char = lit[1];
            
        return c.LLVMConstInt(c.LLVMInt32Type(), char, 0);
    }
};
