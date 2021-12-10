const std = @import("std");
const parser = @import("parser.zig");
const Node = parser.AstNode;
const Parser = parser.Parser;

const CodeGenerator = @This();
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

const space_width = 8;
const space = " ";

const Error = error{
    InvalidExpression,
} || std.os.WriteError;

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

pub fn asmPrologue(self: *CodeGenerator) Error!void {
    try self.output_writer.print(
        \\{[spaces]s:>[width]}.globl main
        \\{[spaces]s:>[width]}.type  main, @function
        \\main:
        \\
    , .{
        .spaces = space,
        .width = space_width,
    });
}

pub fn asmEpilogue(self: *CodeGenerator) Error!void {
    try self.output_writer.print(
        \\
        \\{[spaces]s:>[width]}ret
        \\
    , .{
        .spaces = space,
        .width = space_width,
    });
}

pub fn generateAsm(self: *CodeGenerator, node: *const Node) Error!void {
    //since these nodes are terminal nodes there are no other nodes on either sides of the tree
    //so we must return after generating code for them. These serve as the terminating condition
    //of the recursive descent
    if (node.kind == .NK_NUM) {
        try self.output_writer.print("{[spaces]s:>[width]}mov ${[immediate_constant]d},%rax\n", .{
            .spaces = space,
            .width = space_width,
            .immediate_constant = node.value,
        });
        return;
    }
    if (node.kind == .NK_NEG) {
        try self.generateAsm(node.rhs.?);
        try self.output_writer.print("{[spaces]s:>[width]}neg %rax\n", .{
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
        else => {
            std.log.err("invalid ast expression", .{});
            return error.InvalidExpression;
        },
    }
}
