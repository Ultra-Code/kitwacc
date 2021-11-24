const std = @import("std");
const cwd = std.fs.cwd();

fn compileInt() !void {
    const args = std.process.args;
    var ArgsItr = args();
    var buffer: [100]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var fixed_allocator = &fba.allocator;
    //skip program name
    const program_name = ArgsItr.next(fixed_allocator).?;
    const return_args = if (ArgsItr.next(fixed_allocator)) |argv| argv else {
        std.log.err("Args to zig are null", .{});
        std.log.err("{s} has invalid number of arguments.Expected 1 found 0", .{program_name});
        std.os.exit(1);
    };
    const output = try cwd.createFile("test/output.s", .{});
    defer output.close();
    try output.writer().print(
        \\        .globl main
        \\        .type  main, @function
        \\main:
        \\        mov  ${s}, %rax
        \\        ret
        \\
    , .{return_args});
}

test "compileInt test" {
    //    const slice = &[_:null]?[*:0]const u8{ "gcc", "-o", "output", "output.s" };
    //    const errors = std.os.execvpeZ("gcc", slice, &[_:null]?[*:0]const u8{});
    //    switch (errors) {
    //        error.SystemResources => std.log.info("execvpeZ returned error {}", .{errors}),
    //        _ => std.log.info("some other error occured", .{}),
    //    }
    //    //std.os.ExecveError;
    //    const return_state = std.os.execvpeZ("./zig-out/bin/kitwacc", &[_:null]?[*:0]const u8{}, &[_:null]?[*:0]const u8{});
    var buffer: [2048]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var fixed_allocator = &fba.allocator;
    const return_state = try std.ChildProcess.exec(.{ .allocator = fixed_allocator, .argv = &[_][]const u8{ "gcc", "-o", "test/output", "test/output.s" } });
    std.debug.print("\ngcc exit on stdout with\n{s}", .{return_state.stdout});
    std.debug.print("gcc exit on stderr with\n{s}", .{return_state.stderr});
    const exit_code = try std.ChildProcess.exec(.{ .allocator = fixed_allocator, .argv = &[_][]const u8{ "sh", "test/test_compiler.sh" } });
    std.debug.print("{s}", .{exit_code.stdout});
}

pub fn main() !void {
    try compileInt();
}
