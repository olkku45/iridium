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

var global_putchar_val: u8 = undefined;

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

    fn generateCode(self: *CodeGen, node: Node) c.LLVMValueRef {
        switch (node) {
            .program => |program| {
                //print("program\n", .{});
                _ = generateCode(self, program.program_ast.*);
            },
            .identifier => {
                //print("identifier\n", .{});
                //return identifier(node);
            },
            .argument => {
                //print("argument\n", .{});
                //return argument(arg);
            },
            .literal => {
                //print("literal\n", .{});
                //return literal(node);
            }, 
            .function_call => |func_call| {
                //print("function call\n", .{});
                return functionCall(self, func_call.call_argument);
            },
        }

        const msg = "error";
        return c.LLVMConstString(msg, msg.len, 1);
    }

    pub fn compile(self: *CodeGen, ast: Node) void {
        const ir = generateCode(self, ast);
        _ = c.LLVMBuildRet(self.builder, ir);

        createMain(self);

        var error_msg: [*c]u8 = null;
        if (c.LLVMPrintModuleToFile(self.module, "putchar.ll", &error_msg) != 0) {
            print("An error occured writing to file: {s}\n", .{error_msg});
            c.LLVMDisposeMessage(error_msg);
        }

        c.LLVMDumpModule(self.module);
    }

    fn createMain(self: *CodeGen) void {
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

        const putchar_arg = global_putchar_val;
        const char_val = c.LLVMConstInt(c.LLVMInt32Type(), putchar_arg, 0);
        var putchar_args = [_]c.LLVMValueRef{char_val};

        _ = c.LLVMBuildCall2(
            self.builder,
            putchar_type,
            putchar_func,
            &putchar_args,
            1,
            ""
        );

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

    fn argument(arg: *Node) c.LLVMValueRef {
        return literal(arg.argument.argument_literal.*.literal.value);
    }

    // return the char in the putchar call
    fn literal(lit: []const u8) c.LLVMValueRef {
        // TODO: tokenize characters better, take just the character not the quotes
        const char = lit[1];
        global_putchar_val = char;
            
        return c.LLVMConstInt(c.LLVMInt32Type(), char, 0);
    }

    // no need for identifier yet since we only have 'putchar'
    fn functionCall(self: *CodeGen, arg: *Node) c.LLVMValueRef {
        var param_types = [_]c.LLVMTypeRef{ c.LLVMInt32Type() };
        const func_type = c.LLVMFunctionType(c.LLVMInt32Type(), &param_types, 1, 0);
        const putChar = c.LLVMAddFunction(self.module, "putchar", func_type);

        const func_arg = argument(arg);
        var args = [_]c.LLVMValueRef{ func_arg };

        //const func_ident = identifier(ident);

        return c.LLVMBuildCall2(
            self.builder,
            func_type,
            putChar,
            &args,
            1,
            "",
        );
    }
};
