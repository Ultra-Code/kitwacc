const std = @import("std");
const Asm = @This();
const space_width = 8;
const space = " ";

const Error = error{} || std.os.WriteError;

output_file: std.fs.File,
output_writer: std.fs.File.Writer,
stack_depth: usize = 0,

pub fn init(file: []const u8) !Asm {
    const output_file = try std.fs.cwd().createFile(file, .{});
    return Asm{ .output_file = output_file, .output_writer = output_file.writer() };
}
pub fn deinit(self: *Asm) void {
    self.output_file.close();
    std.debug.assert(self.stack_depth == 0);
}

/// push or put a register on the stack
pub fn push(self: *Asm, register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}push {[register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .register = register,
    });
    self.stack_depth += 1;
}

///pop or remove a registor from the stack
pub fn pop(self: *Asm, register: []const u8) Error!void {
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
pub fn mov(
    self: *Asm,
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
pub fn movzb(self: *Asm, source_byte_register: []const u8, destination_register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}movzb {[source_byte_register]s},{[destination_register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .source_byte_register = source_byte_register,
        .destination_register = destination_register,
    });
}

///load effective address of source and place it in destination
pub fn lea(self: *Asm, offset: usize, source: []const u8, destination: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}lea -{[offset]d}({[source]s}),{[destination]s}\n", .{
        .spaces = space,
        .width = space_width,
        .offset = offset,
        .source = source,
        .destination = destination,
    });
}

/// negate content of register
pub fn neg(self: *Asm, register: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}neg {[register]s}\n", .{
        .spaces = space,
        .width = space_width,
        .register = register,
    });
}

///sub source from destination and store result in destination
pub fn sub(
    self: *Asm,
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
pub fn add(self: *Asm, source: []const u8, destination: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}add {[source]s},{[destination]s}\n", .{
        .spaces = space,
        .width = space_width,
        .source = source,
        .destination = destination,
    });
}

///two operand form of imul
///multiply source by destination and store result in destination
pub fn imul(self: *Asm, source: []const u8, destination: []const u8) Error!void {
    try self.output_writer.print("{[spaces]s:>[width]}imul {[source]s},{[destination]s}\n", .{
        .spaces = space,
        .width = space_width,
        .source = source,
        .destination = destination,
    });
}

pub fn idiv(self: *Asm, divisor: []const u8) Error!void {
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
pub fn cmp(self: *Asm, comptime operand_type: OperandType, left_operand: SourceOperandType(operand_type), right_operand: []const u8) Error!void {
    switch (operand_type) {
        .register => {
            try self.output_writer.print("{[spaces]s:>[width]}cmp {[left_operand]s},{[right_operand]s}\n", .{
                .spaces = space,
                .width = space_width,
                .left_operand = left_operand,
                .right_operand = right_operand,
            });
        },
        .immediate_constant => {
            try self.output_writer.print("{[spaces]s:>[width]}cmp ${[left_operand]d},{[right_operand]s}\n", .{
                .spaces = space,
                .width = space_width,
                .left_operand = left_operand,
                .right_operand = right_operand,
            });
        },
    }
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
pub fn set(self: *Asm, cc: Cc, byte_register: []const u8) Error!void {
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

///ump if Condition Is Met
pub fn jcc(self: *Asm, cc: Cc, byte_register: []const u8) Error!void {
    switch (cc) {
        Cc.equal => {
            try self.output_writer.print("{[spaces]s:>[width]}je {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.not_equal => {
            try self.output_writer.print("{[spaces]s:>[width]}jne {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.less_than => {
            try self.output_writer.print("{[spaces]s:>[width]}jl {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.less_than_equal => {
            try self.output_writer.print("{[spaces]s:>[width]}jle {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.greater_than => {
            try self.output_writer.print("{[spaces]s:>[width]}jg {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
        Cc.greater_than_equal => {
            try self.output_writer.print("{[spaces]s:>[width]}jge {[byte_register]s}\n", .{
                .spaces = space,
                .width = space_width,
                .byte_register = byte_register,
            });
        },
    }
}

pub fn comment(self: *Asm, asm_comment: []const u8) Error!void {
    try self.output_writer.print(
        \\
        \\#{[comments]s}
        \\
    , .{ .comments = asm_comment });
}

pub fn labelFunction(self: *Asm, func_name: []const u8) Error!void {
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
pub fn label(self: *Asm, name: []const u8) Error!void {
    try self.output_writer.print(
        \\
        \\{[label_name]s}:
        \\
    , .{
        .label_name = name,
    });
}
pub fn ret(self: *Asm) Error!void {
    try self.output_writer.print(
        \\{[spaces]s:>[width]}ret
        \\
    , .{
        .spaces = space,
        .width = space_width,
    });
}

pub fn jmp(self: *Asm, location: []const u8) Error!void {
    try self.output_writer.print(
        \\{[space]s:>[width]}jmp {[location]s}
        \\
    , .{
        .space = space,
        .width = space_width,
        .location = location,
    });
}
