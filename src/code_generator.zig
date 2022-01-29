const std = @import("std");
const parser = @import("parser.zig");
const AstNode = parser.AstTree.AstNode;
const Function = parser.Function;
const ExprList = parser.ExprList;

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

/// push or put a register on the stack
fn push(self: *CodeGenerator, register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}push {[register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .register = register,
    });
    self.stack_depth += 1;
}

///pop or remove a registor from the stack
fn pop(self: *CodeGenerator, register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}pop {[register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .register = register,
    });
    self.stack_depth -= 1;
}
const OperandType = enum {
    immediate_constant,
    register,
};

fn SourceOperandType(comptime operand: OperandType) type {
    if (operand == OperandType.register) {
        return []const u8;
    } else {
        return usize;
    }
}

///mov source -> destination
fn mov(
    self: *CodeGenerator,
    comptime operand_type: OperandType,
    source_operand: if (operand_type == OperandType.register) []const u8 else usize,
    destination_register: []const u8,
) Error!void {
    switch (operand_type) {
        .immediate_constant => try self.output_writer.print("{[spaces]s:>[width]}mov ${[immediate_constant]d},{[register]s}\n", .{
            .spaces = space,
            .width = space_width,
            .immediate_constant = source_operand,
            .register = destination_register,
        }),
        .register => {
            try self.output_writer.print("{[spaces]s:>[width]}mov {[source]s},{[destination]s}\n", .{
                .spaces = space,
                .width = space_width,
                .source = source_operand,
                .destination = destination_register,
            });
        },
    }
}

///Copies the contents of the source operand (register or memory location) to the destination operand (register) and
///zero extends the value.
fn movzb(self: *CodeGenerator, source_byte_register: []const u8, destination_register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}movzb {[source_byte_register]s},{[destination_register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .source_byte_register = source_byte_register,
        .destination_register = destination_register,
    });
}

///load effective address of source and place it in destination
fn lea(self: *CodeGenerator, offset: usize, source: []const u8, destination: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}lea -{[offset]d}({[source]s}),{[destination]s}\n", .{
        .spaces = space,
        .width = space_width,
        .offset = offset,
        .source = source,
        .destination = destination,
    });
}

/// negate content of register
fn neg(self: *CodeGenerator, register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}neg {[register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .register = register,
    });
}

///sub source from destination and store result in destination
fn sub(
    self: *CodeGenerator,
    comptime operand_type: OperandType,
    source_operand: SourceOperandType(operand_type),
    destination: []const u8,
) Error!void {
    switch (operand_type) {
        .register => {
            try self.output_writer.print("{[spaces]s:>[width]}sub {[source]s},{[destination]s}\n", .{
                .spaces = space,
                .width = space_width,
                .source = source_operand,
                .destination = destination,
            });
        },
        .immediate_constant => {
            try self.output_writer.print("{[spaces]s:>[width]}sub ${[immediate_constant]d},{[destination]s}\n", .{
                .spaces = space,
                .width = space_width,
                .immediate_constant = source_operand,
                .destination = destination,
            });
        },
    }
}

///add source to destination and store result in destination
fn add(self: *CodeGenerator, source: []const u8, destination: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}add {[source]s},{[destination]s}\n", .{
        .spaces = space,
        .width = space_width,
        .source = source,
        .destination = destination,
    });
}

///two operand form of imul
///multiply source by destination and store result in destination
fn imul(self: *CodeGenerator, source: []const u8, destination: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}imul {[source]s},{[destination]s}\n", .{
        .spaces = space,
        .width = space_width,
        .source = source,
        .destination = destination,
    });
}

fn idiv(self: *CodeGenerator, divisor: []const u8) Error!void {
    //https://stackoverflow.com/questions/38416593/why-should-edx-be-0-before-using-the-div-instruction/38416896#38416896
    //we are dividing a 128 dividend bit number by a 64 bit divisor number
    //so we must sign extend %rax into 128 bit RDX:RAX before dividing RDX:RAX with %rdi
    //sign extending extends the MSB into double the current size of the register
    //this makes sure all 128 bits in RDX:RAX are set before performing idiv instruction
    //else the result would be undefined
    try self.output_writer.print(
        \\{[spaces]s:>[width]}cqo
        \\{[spaces]s:>[width]}idiv {[divisor]s}
        \\
    , .{
        .spaces = space,
        .width = space_width,
        .divisor = divisor,
    });
}

///Compares the first source operand with the second source operand and
///sets the status flags in the EFLAGS register according to the result
///The condition codes used by the Jcc, CMOVcc, and SETcc instructions
///are based on the results of a CMP instruction.
fn cmp(self: *CodeGenerator, operand_1: []const u8, operand_2: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}cmp {[operand_1]s},{[operand_2]s}\n", .{
        .spaces = space,
        .width = space_width,
        .operand_1 = operand_1,
        .operand_2 = operand_2,
    });
}

const Cc = enum {
    equal,
    not_equal,
    less_than,
    less_than_equal,
    greater_than,
    greater_than_equal,
};

///set byte register base on conditional codes of  EFLAGS
fn set(self: *CodeGenerator, cc: Cc, byte_register: []const u8) Error!void {
    switch (cc) {
        Cc.equal => {
            try self.output_writer.print("{[spaces]s:>[width]}sete {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.not_equal => {
            try self.output_writer.print("{[spaces]s:>[width]}setne {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.less_than => {
            try self.output_writer.print("{[spaces]s:>[width]}setl {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.less_than_equal => {
            try self.output_writer.print("{[spaces]s:>[width]}setle {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.greater_than => {
            try self.output_writer.print("{[spaces]s:>[width]}setg {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.greater_than_equal => {
            try self.output_writer.print("{[spaces]s:>[width]}setge {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
    }
}
fn comment(self: *CodeGenerator, asm_comment: []const u8) Error!void {
    try self.output_writer.print(
        \\
        \\#{[comments]s}
        \\
    , .{ .comments = asm_comment });
}

fn labelFunction(self: *CodeGenerator, func_name: []const u8) Error!void {
    try self.output_writer.print(
        \\{[spaces]s:>[width]}.globl {[func_name]s}
        \\{[spaces]s:>[width]}.type  {[func_name]s}, @function
    , .{
        .spaces = space,
        .width = space_width,
        .func_name = func_name,
    });
    try self.label(func_name);
}

///Add a label in asm output
fn label(self: *CodeGenerator, name: []const u8) Error!void {
    try self.output_writer.print(
        \\
        \\{[label_name]s}:
        \\
    , .{
        .label_name = name,
    });
}

fn asmPrologue(self: *CodeGenerator, stack_size: usize) Error!void {
    // Prologue
    try self.comment("global main entry point ");
    try self.labelFunction("main");
    try self.comment("asm prologue");

    try self.push("%rbp");
    try self.mov(.register, "%rsp", "%rbp");
    try self.sub(.immediate_constant, stack_size, "%rsp");
}

fn ret(self: *CodeGenerator) Error!void {
    try self.output_writer.print(
        \\{[spaces]s:>[width]}ret
        \\
    , .{
        .spaces = space,
        .width = space_width,
    });
}

fn jmp(self: *CodeGenerator, location: []const u8) Error!void {
    try self.output_writer.print(
        \\{[space]s:>[width]}jmp {[location]s}
        \\
    , .{
        .space = space,
        .width = space_width,
        .location = location,
    });
}

fn asmEpilogue(self: *CodeGenerator) Error!void {
    try self.comment("asm epilogue");
    try self.label(".L.exit_main");
    try self.mov(.register, "%rbp", "%rsp");
    try self.pop("%rbp");
    try self.ret();
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
fn genAbsoluteAddress(self: *CodeGenerator, node: *const AstNode) Error!void {
    if (node.kind == .NK_VAR) {
        try self.lea(node.value.identifier.rbp_offset, "%rbp", "%rax");
        return;
    }
    std.log.err("Not an lvalue", .{});
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

fn genStmts(self: *CodeGenerator, nodes: *ExprList.Node) Error!void {
    const node = nodes.data;
    if (node.kind == .NK_BLOCK) {
        // Traverse forwards.
        //the data for the block is the expression field of the Value union
        var it = nodes.data.value.expression.iterator();
        while (it.next()) |block| {
            try self.genStmts(block);
        }
        return;
    }
    if (node.kind == .NK_RETURN) {
        try self.generateAsm(node.rhs.?);
        try self.jmp(".L.exit_main");
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
        try self.mov(.immediate_constant, node.value.number, "%rax");
        return;
    }
    if (node.kind == .NK_NEG) {
        //recurse on the side of the tree were the nodes are
        try self.generateAsm(node.rhs.?);
        try self.neg("%rax");
        return;
    }
    if (node.kind == .NK_VAR) {
        try self.genAbsoluteAddress(node);
        try self.mov(.register, "(%rax)", "%rax");
        return;
    }
    if (node.kind == .NK_ASSIGN) {
        try self.genAbsoluteAddress(node.lhs.?);
        try self.push("%rax");
        try self.generateAsm(node.rhs.?);
        try self.pop("%rdi");
        try self.mov(.register, "%rax", "(%rdi)");
        return;
    }
    if (node.kind == .NK_RETURN) {
        try self.generateAsm(node.rhs.?);
    }
    try self.generateAsm(node.rhs.?);
    try self.push("%rax");
    try self.generateAsm(node.lhs.?);
    try self.pop("%rdi");

    switch (node.kind) {
        .NK_ADD => try self.add("%rdi", "%rax"),
        .NK_SUB => try self.sub(.register, "%rdi", "%rax"),
        .NK_MUL => try self.imul("%rdi", "%rax"),
        .NK_DIV => try self.idiv("%rdi"),
        .NK_EQ, .NK_NE, .NK_LT, .NK_LE, .NK_GE, .NK_GT => {
            try self.cmp("%rdi", "%rax");

            if (node.kind == .NK_EQ) {
                try self.set(Cc.equal, "%al");
            } else if (node.kind == .NK_NE) {
                try self.set(Cc.not_equal, "%al");
            } else if (node.kind == .NK_LT) {
                try self.set(Cc.less_than, "%al");
            } else if (node.kind == .NK_LE) {
                try self.set(Cc.less_than_equal, "%al");
            } else if (node.kind == .NK_GT) {
                try self.set(Cc.greater_than, "%al");
            } else if (node.kind == .NK_GE) {
                try self.set(Cc.greater_than_equal, "%al");
            }

            try self.movzb("%al", "%rax");
        },
        else => {
            std.log.err("invalid ast node expression", .{});
            return error.InvalidExpression;
        },
    }
}
