const std = @import("std");
const cwd = std.fs.cwd();

fn compileInt(allocator: *std.mem.Allocator, args: []const []const u8) !void {
    const program_name = args[0];
    std.log.info("argv is {s}", .{args});
    if (args.len != 2) {
        std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
        return error.InvalidNumberOfArguments;
    }

    const output = try std.fs.cwd().createFile("test/output.s", .{});
    defer output.close();
    const first_operand = try ArgsItr.next(fixed_allocator).?;
    const space = " ";
    try output.writer().print(
        \\{0s:>8}.globl main
        \\{0s:>8}.type  main, @function
        \\main:
        \\{0s:>8}mov ${1s}, %rax
        \\
    , .{ space, first_operand });
    while (ArgsItr.next(fixed_allocator)) |argv| {
        if (argv) |value| {
            if (std.mem.eql(u8, value, "+")) {
                const operand = try ArgsItr.next(fixed_allocator).?;
                try output.writer().print("{s:>8}add ${s}, %rax", .{ space, operand });
                continue;
            } else if (std.mem.eql(u8, value, "-")) {
                const operand = try ArgsItr.next(fixed_allocator).?;
                try output.writer().print("{s:>8}sub ${s}, %rax", .{ space, operand });
                continue;
            } else {
                std.log.err("Unexpected caracter: {s}", .{value});
                std.os.exit(1);
            }
        } else |err| {
            std.log.err("Args to zig are {}", .{err});
            std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
            std.os.exit(1);
        }
    }
    try output.writer().print(
        \\
        \\{s:>8}ret
        \\
    , .{space});
}

test "compileInt test" {
    var buffer: [2048]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var fixed_allocator = &fba.allocator;
    const exit_code = try std.ChildProcess.exec(.{ .allocator = fixed_allocator, .argv = &[_][]const u8{"./test/test_compiler"} });
    std.debug.print("{s}\n{s}", .{ exit_code.stderr, exit_code.stdout });
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) {
        std.log.err("Memory Leaks Deteckted", .{});
    };
    var gpa_allocator = &gpa.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    compileInt(allocator, args) catch |err|
        switch (err) {
        error.InvalidNumberOfArguments => std.log.err("{s} call kitwacc with 1 argument", .{@errorName(err)}),
        else => std.log.err("{s} error occured", .{@errorName(err)}),
    };
}
