const std = @import("std");
const parser = @import("parser.zig");
const Node = parser.AstNode;
const Parser = parser.Parser;
const Function = parser.Function;

const CodeGenerator = @This();

const Error = error{
    InvalidExpression,
} || std.os.WriteError;

const space_width = 8;
const space = " ";

stack_depth: u32,
output_file: std.fs.File,
output_writer: std.fs.File.Writer = undefined,

pub fn init(file: []const u8) !CodeGenerator {
    const output_file = try std.fs.cwd().createFile(file, .{});
    return CodeGenerator{ .stack_depth = 0, .output_file = output_file, .output_writer = output_file.writer() };
}

pub fn deinit(self: *CodeGenerator) void {
    self.output_file.close();
    std.debug.assert(self.stack_depth == 0);
}

fn push(self: *CodeGenerator) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}push %rax\n", .{
        .spaces = space,
        .width = space_width,
    });
    self.stack_depth += 1;
}

fn pop(self: *CodeGenerator, register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}pop {[register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .register = register,
    });
    self.stack_depth -= 1;
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
fn genAbsoluteAddress(self: *CodeGenerator, node: *const Node) Error!void {
    if (node.kind == .NK_VAR) {
        try self.output_writer.print("{[spaces]s:>[width]}lea -{[offset]d}(%rbp) , %rax\n", .{
            .spaces = space,
            .width = space_width,
            .offset = node.value.identifier.rbp_offset,
        });
        return;
    }
    std.log.err("Not an lvalue", .{});
}

pub fn codegen(self: *CodeGenerator, fn_nodes: Function) Error!void {
    try self.asmPrologue(fn_nodes.stack_size);
    for (fn_nodes.body) |node| {
        try self.genStmts(node);
    }
    try self.asmEpilogue();
}
fn genStmts(self: *CodeGenerator, node: *const Node) Error!void {
    if (node.kind == .NK_EXPR_STMT) {
        try self.generateAsm(node.lhs.?);
        return;
    }
    std.log.err("invalid expression statement", .{});
}

fn asmPrologue(self: *CodeGenerator, stack_size: usize) Error!void {
    // Prologue
    try self.output_writer.print(
        \\#global main entry point
        \\{[spaces]s:>[width]}.globl main
        \\{[spaces]s:>[width]}.type  main, @function
        \\main:
        \\#prologue
        \\{[spaces]s:>[width]}push %rbp
        \\{[spaces]s:>[width]}mov %rsp, %rbp
        \\{[spaces]s:>[width]}sub ${[stack_offset]d},%rsp
        \\
        \\
    , .{
        .spaces = space,
        .width = space_width,
        .stack_offset = stack_size,
    });
}

fn asmEpilogue(self: *CodeGenerator) Error!void {
    try self.output_writer.print(
        \\#epilogue
        \\{[spaces]s:>[width]}mov %rbp, %rsp
        \\{[spaces]s:>[width]}pop %rbp
        \\{[spaces]s:>[width]}ret
        \\
    , .{
        .spaces = space,
        .width = space_width,
    });
}

// Generate code for a given node.
fn generateAsm(self: *CodeGenerator, node: *const Node) Error!void {
    //since these nodes are terminal nodes there are no other nodes on either sides of the tree
    //so we must return after generating code for them. These serve as the terminating condition
    //of the recursive descent
    if (node.kind == .NK_NUM) {
        try self.output_writer.print("{[spaces]s:>[width]}mov ${[immediate_constant]d},%rax\n", .{
            .spaces = space,
            .width = space_width,
            .immediate_constant = node.value.number,
        });
        return;
    }
    if (node.kind == .NK_NEG) {
        //recurse on the side of the tree were the nodes are
        try self.generateAsm(node.lhs.?);
        try self.output_writer.print("{[spaces]s:>[width]}neg %rax\n", .{
            .spaces = space,
            .width = space_width,
        });
        return;
    }
    if (node.kind == .NK_VAR) {
        try self.genAbsoluteAddress(node);
        try self.output_writer.print("{[spaces]s:>[width]}mov (%rax), %rax\n", .{
            .spaces = space,
            .width = space_width,
        });
        return;
    }
    if (node.kind == .NK_ASSIGN) {
        try self.genAbsoluteAddress(node.lhs.?);
        try self.push();
        try self.generateAsm(node.rhs.?);
        try self.pop("%rdi");
        try self.output_writer.print("{[spaces]s:>[width]}mov %rax, (%rdi)\n", .{
            .spaces = space,
            .width = space_width,
        });
        return;
    }
    try self.generateAsm(node.rhs.?);
    try self.push();
    try self.generateAsm(node.lhs.?);
    try self.pop("%rdi");

    switch (node.kind) {
        .NK_ADD => try self.output_writer.print("{[spaces]s:>[width]}add %rdi,%rax\n", .{
            .spaces = space,
            .width = space_width,
        }),
        .NK_SUB => try self.output_writer.print("{[spaces]s:>[width]}sub %rdi,%rax\n", .{
            .spaces = space,
            .width = space_width,
        }),
        .NK_MUL => try self.output_writer.print("{[spaces]s:>[width]}imul %rdi,%rax\n", .{
            .spaces = space,
            .width = space_width,
        }),
        .NK_DIV => {
            try self.output_writer.print("{[spaces]s:>[width]}cqo\n", .{
                .spaces = space,
                .width = space_width,
            });
            try self.output_writer.print("{[spaces]s:>[width]}idiv %rdi\n", .{
                .spaces = space,
                .width = space_width,
            });
        },
        .NK_EQ, .NK_NE, .NK_LT, .NK_LE, .NK_GE, .NK_GT => {
            try self.output_writer.print("{[spaces]s:>[width]}cmp %rdi,%rax\n", .{
                .spaces = space,
                .width = space_width,
            });
            if (node.kind == .NK_EQ) {
                try self.output_writer.print("{[spaces]s:>[width]}sete %al\n", .{
                    .spaces = space,
                    .width = space_width,
                });
            } else if (node.kind == .NK_NE) {
                try self.output_writer.print("{[spaces]s:>[width]}setne %al\n", .{
                    .spaces = space,
                    .width = space_width,
                });
            } else if (node.kind == .NK_LT) {
                try self.output_writer.print("{[spaces]s:>[width]}setl %al\n", .{
                    .spaces = space,
                    .width = space_width,
                });
            } else if (node.kind == .NK_LE) {
                try self.output_writer.print("{[spaces]s:>[width]}setle %al\n", .{
                    .spaces = space,
                    .width = space_width,
                });
            } else if (node.kind == .NK_GE) {
                try self.output_writer.print("{[spaces]s:>[width]}setge %al\n", .{
                    .spaces = space,
                    .width = space_width,
                });
            } else if (node.kind == .NK_GT) {
                try self.output_writer.print("{[spaces]s:>[width]}setg %al\n", .{
                    .spaces = space,
                    .width = space_width,
                });
            }

            try self.output_writer.print("{[spaces]s:>[width]}movzb %al,%rax\n", .{
                .spaces = space,
                .width = space_width,
            });
        },
        else => {
            std.log.err("invalid ast node expression", .{});
            return error.InvalidExpression;
        },
    }
}
