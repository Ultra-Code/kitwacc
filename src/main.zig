const std = @import("std");
const cwd = std.fs.cwd();
const tokenizers = @import("tokenizer.zig");
const Tokenizer = tokenizers.Tokenizer;

fn compileInt(allocator: *std.mem.Allocator, args: []const []const u8) !void {
    const program_name = args[0];
    std.log.info("argv is {s}", .{args});
    if (args.len != 2) {
        std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
        return error.InvalidNumberOfArguments;
    }

    const output = try std.fs.cwd().createFile("test/output.s", .{});
    defer output.close();

    var tokenizer = Tokenizer.init(allocator);
    var token = try tokenizer.tokenize(args[1]);

    const space = " ";
    try output.writer().print(
        \\{0s:>8}.globl main
        \\{0s:>8}.type  main, @function
        \\main:
        \\{0s:>8}mov ${1d}, %rax
        \\
    , .{ space, token.value }); // The first token must be a number

    token = tokenizer.nextToken();

    // ... followed by either `+ <number>` or `- <number>`.
    while (token.kind != .TK_EOF) : (token = tokenizer.nextToken()) {
        if (Tokenizer.match(token, "+")) {
            token = tokenizer.nextToken();
            try output.writer().print("{s:>8}add ${d}, %rax\n", .{ space, token.value });
            continue;
        }
        if (Tokenizer.match(token, "-")) {
            token = tokenizer.nextToken();
            try output.writer().print("{s:>8}sub ${d}, %rax\n", .{ space, token.value });
            continue;
        }

        tokenizers.reportError(args[1], "^ expected an operator", token);
        std.process.exit(1);
    }

    try output.writer().print(
        \\
        \\{s:>8}ret
        \\
    , .{space});
}

test "compileInt test" {
    var buffer: [10240]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var fixed_allocator = &fba.allocator;
    const exit_code = try std.ChildProcess.exec(.{ .allocator = fixed_allocator, .argv = &[_][]const u8{"./test/test-compiler"} });
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
