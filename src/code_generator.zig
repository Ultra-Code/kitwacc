const std = @import("std");
const parser = @import("parser.zig");
const AstNode = parser.AstTree.AstNode;
const Function = parser.Function;
const ExprList = parser.ExprList;

const CodeGenerator = @This();

const Asm = @import("asm.zig");

const Error = error{
    InvalidExpression,
} || std.os.WriteError;

asm_: Asm,
label_count: usize = 0,

pub fn init(file: []const u8) !CodeGenerator {
    const asm_printer = try Asm.init(file);
    return CodeGenerator{ .asm_ = asm_printer };
}

pub fn deinit(self: *CodeGenerator) void {
    self.asm_.deinit();
}

pub fn codegen(self: *CodeGenerator, fn_nodes: Function) Error!void {
    try self.asmPrologue(fn_nodes.stack_size);
    // Traverse forwards.
    var it = fn_nodes.body.iterator();
    while (it.next()) |node| {
        try self.genStmts(node);
    }
    try self.asmEpilogue();
}

fn incLabelCount(self: *CodeGenerator) usize {
    self.label_count += 1;
    return self.label_count;
}

fn genStmts(self: *CodeGenerator, node: *const AstNode) Error!void {
    if (node.kind == .NK_IF) {
        const label_num = self.incLabelCount();
        const if_statement = node.value.if_statement;
        try self.generateAsm(if_statement.if_expr);
        try self.asm_.cmp(.immediate_constant, 0, "%rax");

        var else_label_buf: [16]u8 = undefined;
        const else_label = try std.fmt.bufPrint(&else_label_buf, ".L.else.{d}", .{label_num});
        var end_label_buf: [16]u8 = undefined;
        const end_label = try std.fmt.bufPrint(&end_label_buf, ".L.end_if.{d}", .{label_num});

        //if rax == 0 means if expr is false so jump to else branch
        try self.asm_.jcc(.equal, else_label);
        try self.genStmts(if_statement.then_branch);
        try self.asm_.jmp(end_label);
        try self.asm_.label(else_label);
        if (if_statement.else_branch) |else_branch| {
            try self.genStmts(else_branch);
        }
        try self.asm_.label(end_label);
        return;
    }
    if (node.kind == .NK_BLOCK) {
        //Traverse forwards.
        //the data for the block is the block field of the Value union
        var it = node.value.block.iterator();

        while (it.next()) |block| {
            try self.genStmts(block);
        }
        return;
    }
    if (node.kind == .NK_RETURN) {
        try self.generateAsm(node.rhs.?);
        try self.asm_.jmp(".L.exit_main");
        return;
    }
    if (node.kind == .NK_EXPR_STMT) {
        try self.generateAsm(node.rhs.?);
        return;
    }
    std.log.err("invalid expression statement", .{});
}

// Generate code for a given node.
fn generateAsm(self: *CodeGenerator, node: *const AstNode) Error!void {
    //since these nodes are terminal nodes there are no other nodes on either sides of the tree
    //so we must return after generating code for them. These serve as the terminating condition
    //of the recursive descent
    if (node.kind == .NK_NUM) {
        try self.asm_.mov(.immediate_constant, node.value.number, "%rax");
        return;
    }
    if (node.kind == .NK_NEG) {
        //recurse on the side of the tree were the nodes are
        try self.generateAsm(node.rhs.?);
        try self.asm_.neg("%rax");
        return;
    }
    if (node.kind == .NK_VAR) {
        try self.genAbsoluteAddress(node);
        try self.asm_.mov(.register, "(%rax)", "%rax");
        return;
    }
    if (node.kind == .NK_ASSIGN) {
        try self.genAbsoluteAddress(node.lhs.?);
        try self.asm_.push("%rax");
        try self.generateAsm(node.rhs.?);
        try self.asm_.pop("%rdi");
        try self.asm_.mov(.register, "%rax", "(%rdi)");
        return;
    }
    if (node.kind == .NK_RETURN) {
        try self.generateAsm(node.rhs.?);
    }
    try self.generateAsm(node.rhs.?);
    try self.asm_.push("%rax");
    try self.generateAsm(node.lhs.?);
    try self.asm_.pop("%rdi");

    switch (node.kind) {
        .NK_ADD => try self.asm_.add("%rdi", "%rax"),
        .NK_SUB => try self.asm_.sub(.register, "%rdi", "%rax"),
        .NK_MUL => try self.asm_.imul("%rdi", "%rax"),
        .NK_DIV => try self.asm_.idiv("%rdi"),
        .NK_EQ, .NK_NE, .NK_LT, .NK_LE, .NK_GE, .NK_GT => {
            try self.asm_.cmp(.register, "%rdi", "%rax");

            if (node.kind == .NK_EQ) {
                try self.asm_.set(.equal, "%al");
            } else if (node.kind == .NK_NE) {
                try self.asm_.set(.not_equal, "%al");
            } else if (node.kind == .NK_LT) {
                try self.asm_.set(.less_than, "%al");
            } else if (node.kind == .NK_LE) {
                try self.asm_.set(.less_than_equal, "%al");
            } else if (node.kind == .NK_GT) {
                try self.asm_.set(.greater_than, "%al");
            } else if (node.kind == .NK_GE) {
                try self.asm_.set(.greater_than_equal, "%al");
            }

            try self.asm_.movzb("%al", "%rax");
        },
        else => {
            std.log.err("invalid ast node expression", .{});
            return error.InvalidExpression;
        },
    }
}
fn asmPrologue(self: *CodeGenerator, stack_size: usize) Error!void {
    // Prologue
    try self.asm_.comment("global main entry point ");
    try self.asm_.labelFunction("main");
    try self.asm_.comment("asm prologue");

    try self.asm_.push("%rbp");
    try self.asm_.mov(.register, "%rsp", "%rbp");
    try self.asm_.sub(.immediate_constant, stack_size, "%rsp");
}

fn asmEpilogue(self: *CodeGenerator) Error!void {
    try self.asm_.comment("asm epilogue");
    try self.asm_.label(".L.exit_main");
    try self.asm_.mov(.register, "%rbp", "%rsp");
    try self.asm_.pop("%rbp");
    try self.asm_.ret();
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
fn genAbsoluteAddress(self: *CodeGenerator, node: *const AstNode) Error!void {
    if (node.kind == .NK_VAR) {
        try self.asm_.lea(node.value.identifier.rbp_offset, "%rbp", "%rax");
        return;
    }
    std.log.err("Not an lvalue", .{});
}
