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
        //declareFunctions(self);
        // README: the ast is traversed in createMain()
        //createMain(self, ast);
        // new:
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

    // TODO: remove
    fn declareFunctions(self: *CodeGen) void {
        declarePutchar(self);
    }

    fn generateProgramCode(self: *CodeGen, ast: Node) !void {
        for (ast.program.program_ast.items) |node_ptr| {
            switch (node_ptr.*) {
                .extern_fn_decl => {
                    declarePutchar(self);
                },
                .function_decl => |declaration| {
                    try createFunction(self, declaration.name, node_ptr);
                },
                else => {},
            }
        }
    }

    // TODO: remove
    fn traverseAst(self: *CodeGen, ast: Node) void {
        for (ast.program.program_ast.items) |node_ptr| {
            generateCode(self, node_ptr);
        }
    }

    // TODO: remove
    fn generateCode(self: *CodeGen, node_ptr: *Node) void {
        switch (node_ptr.*) {
            .function_call => |call| {
                const arg = call.func_argument.*;
                const char = arg.literal.value[1];
                createPutcharCall(self, char);
            },
            else => {},
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

    // TODO: remove after generateCode knows how to make a main function,
    // that is after you can make a main function in the code
    fn createMain(self: *CodeGen, ast: Node) void {
        const main_type = c.LLVMFunctionType(c.LLVMInt32Type(), null, 0, 0);
        const main = c.LLVMAddFunction(self.module, "main", main_type);

        const entry = c.LLVMAppendBasicBlock(main, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        traverseAst(self, ast); // just generate putchar-calls for now

        const ret_val = c.LLVMConstInt(c.LLVMInt32Type(), 0, 0);

        _ = c.LLVMBuildRet(self.builder, ret_val);
    }

    // create main function
    fn createFunction(self: *CodeGen, name: []const u8, decl: *Node) !void {
        // TODO: make 'self' have allocator
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();

        // add null termination
        const name_copy = try std.fmt.allocPrint(alloc, "{s}", .{name});
        defer alloc.free(name_copy);
        
        const func_type = c.LLVMFunctionType(c.LLVMInt32Type(), null, 0, 0);
        const func = c.LLVMAddFunction(self.module, name_copy.ptr, func_type);

        const block = c.LLVMAppendBasicBlock(func, "block");
        c.LLVMPositionBuilderAtEnd(self.builder, block);

        for (decl.*.function_decl.body.items) |statement| {
            switch (statement.*) {
                .function_call => |call| {
                    const putchar_arg = call.func_argument.*;
                    const putchar_char = putchar_arg.literal.value[1];
                    createPutcharCall(self, putchar_char);
                },
                else => {},
            }
        } 

        const ret_val = c.LLVMConstInt(c.LLVMInt32Type(), 0, 0);

        _ = c.LLVMBuildRet(self.builder, ret_val);
    }
};
