const std = @import("std");
const cwd = std.fs.cwd();
const Parser = @import("parser.zig");
const Tokenizer = @import("tokenizer.zig");
const CodeGenerator = @import("code_generator.zig");

pub var TOKEN_STREAM: []const u8 = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) {
        std.log.err("Memory Leaks Deteckted", .{});
    };
    const gpa_allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const argv = try std.process.argsAlloc(allocator);

    const program_name = argv[0];
    TOKEN_STREAM = argv[1];
    if (argv.len < 2) {
        std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
        std.log.err("{s} call kitwacc with 1 argument", .{program_name});
        return error.InvalidNumberOfArguments;
    }
    compiler(allocator) catch |err|
        switch (err) {
        else => std.log.err("{s} error occured", .{@errorName(err)}),
    };
}

fn compiler(allocator: std.mem.Allocator) !void {
    // Tokenize and parse.
    var tokenized_input = try Tokenizer.init(allocator).tokenize();
    var ast_nodes = Parser.init(allocator, tokenized_input).parse();

    var generator = try CodeGenerator.init("test/output.s");
    defer generator.deinit();
    // Traverse the AST to emit assembly.
    try generator.codegen(ast_nodes);
}

test "kitwacc compiler test suite" {
    const exit_code = try std.ChildProcess.exec(.{ .allocator = std.testing.allocator, .argv = &[_][]const u8{"./test/test-compiler"} });
    std.debug.print("{s}\n{s}", .{ exit_code.stderr, exit_code.stdout });
    std.testing.allocator.free(exit_code.stdout);
    std.testing.allocator.free(exit_code.stderr);
}
