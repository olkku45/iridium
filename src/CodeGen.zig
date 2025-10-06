const std = @import("std");
const Parser = @import("Parser.zig").Parser;
const Node = @import("Parser.zig").Node;

const print = std.debug.print;

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/ExecutionEngine.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
});

pub const CodeGen = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,

    pub fn init() CodeGen {
        const context = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext("TODO", context);
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

    pub fn generateCode(node: Node) void {
        switch (node) {
            .program => |thing| {
                print("program\n", .{});
                generateCode(thing.program_ast.*);
            },
            .identifier => print("identifier\n", .{}), // TODO: generate LLVM type
            .argument => |thing| {
                print("argument\n", .{});
                generateCode(thing.argument_literal.*);
            },
            .literal => print("literal\n", .{}), // TODO: generate LLVM type
            .function_call => |thing| {
                print("function call\n", .{});
                generateCode(thing.call_argument.*);
                generateCode(thing.identifier.*);
            },
        }
    }
};
