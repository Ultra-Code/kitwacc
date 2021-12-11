const std = @import("std");
const cwd = std.fs.cwd();
const parser = @import("parser.zig");
const code_generator = @import("code_generator.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) {
        std.log.err("Memory Leaks Deteckted", .{});
    };
    const gpa_allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try TokenStream.init(allocator);

    const program_name = args.input[0];
    const stream = args.input[1];
    if (args.input.len < 2) {
        std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
        std.log.err("{s} call kitwacc with 1 argument", .{program_name});
        return error.InvalidNumberOfArguments;
    }
    compiler(allocator, stream) catch |err|
        switch (err) {
        else => std.log.err("{s} error occured", .{@errorName(err)}),
    };
}

const TokenStream = struct {
    input: []const []const u8,

    /// The caller must free the returned slice
    fn init(allocator: std.mem.Allocator) !TokenStream {
        const args = try std.process.argsAlloc(allocator);
        return TokenStream{ .input = args };
    }
};

fn compiler(allocator: std.mem.Allocator, stream: []const u8) !void {
    var Parser = parser.init(allocator, stream);
    // Tokenize and parse.
    const token = try Parser.tokenizeInput();
    const ast_node = Parser.parse(token);

    if (Parser.currentToken().kind == .TK_EOF) {
        var generator = try code_generator.init("test/output.s");
        defer generator.deinit();
        // Traverse the AST to emit assembly.
        try generator.codegen(ast_node);
    } else {
        Parser.reportParserError("epected + - < <= > >= == != after {s} but found {s}", .{
            Parser.previousToken().lexeme,
            Parser.currentToken().lexeme,
        });
    }
}

test "compileInt test" {
    const exit_code = try std.ChildProcess.exec(.{ .allocator = std.testing.allocator, .argv = &[_][]const u8{"./test/test-compiler"} });
    std.debug.print("{s}\n{s}", .{ exit_code.stderr, exit_code.stdout });
    std.testing.allocator.free(exit_code.stdout);
    std.testing.allocator.free(exit_code.stderr);
}
