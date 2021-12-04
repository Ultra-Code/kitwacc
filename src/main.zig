const std = @import("std");
const cwd = std.fs.cwd();
const tokenizers = @import("tokenizer.zig");
const Tokenizer = tokenizers.Tokenizer;

const TokenStream = struct {
    input: []const []const u8,

    /// The caller must free the returned slice
    fn init(allocator: *std.mem.Allocator) !TokenStream {
        const args = try std.process.argsAlloc(allocator);
        return TokenStream{ .input = args };
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) {
        std.log.err("Memory Leaks Deteckted", .{});
    };
    var gpa_allocator = &gpa.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    const args = try TokenStream.init(allocator);

    const program_name = args.input[0];
    const stream = args.input[1];
    std.log.info("argv is {s}", .{stream});
    if (args.input.len < 2) {
        std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
        std.log.err("{s} call kitwacc with 1 argument", .{program_name});
        return error.InvalidNumberOfArguments;
    }
    compileInt(allocator, stream) catch |err|
        switch (err) {
        else => std.log.err("{s} error occured", .{@errorName(err)}),
    };
}

fn compileInt(allocator: *std.mem.Allocator, stream: []const u8) !void {
    const output = try std.fs.cwd().createFile("test/output.s", .{});
    defer output.close();

    var tokenizer = Tokenizer.init(allocator, stream);
    var token = try tokenizer.tokenize();

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
        if (tokenizer.match(token, "+")) |next_token| {
            const number = try tokenizer.getNumber();
            try output.writer().print("{s:>8}add ${d}, %rax\n", .{ space, number });
            continue;
        }

        if (tokenizer.match(token, "-")) |next_token| {
            const number = try tokenizer.getNumber();
            try output.writer().print("{s:>8}sub ${d}, %rax\n", .{ space, number });
            continue;
        }

        tokenizer.reportError("expected token {s} to match terminal + or -", .{token.lexeme});
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
