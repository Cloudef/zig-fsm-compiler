const std = @import("std");
const compiler = @import("fsm-compiler");
const graphviz = @import("graphviz.zig");

pub const std_options: std.Options = .{
    .log_level = .info,
    .log_scope_levels = &.{
        .{ .scope = .mutable_machine, .level = .debug },
    },
};

fn version() !void {
    const writer = std.io.getStdOut().writer();
    try writer.print(
        \\{s} version {s}
        \\Copyright (c) 2024 by Vetoniemi Jari Juhani
    , .{ "zig-fsm-compiler", "1.0.0" });
}

fn usage(writer: std.fs.File.Writer) !void {
    try writer.print(
        \\usage: {s} [options] [file]
        \\general:
        \\   -h, -H, -?, --help   Print this usage and exit
        \\   -v, --version        Print version information and exit
        \\   -o <file>            Write output to <file>
        \\   -s                   Print some statistics on stderr
        \\   -I <dir>             Add <dir> to the list of directories to search
        \\                        for included an imported files
        \\error reporting format:
        \\   --error-format=gnu   file:line:column: message (default)
        \\   --error-format=msvc  file(line,column): message
        \\fsm minimization:
        \\   -n                   Do not perform minimization
        \\visualization:
        \\   -V                   Generate a dot file for Graphviz
        \\   -p                   Display printable characters on labels
        \\   -S <spec>            FSM specification to output (for graphviz output)
        \\   -M <machine>         Machine definition/instantiation to output (for graphviz output)
    , .{"zig-fsm-compiler"});
}

fn graphvizExport(allocator: std.mem.Allocator, result: compiler.Result, machine: []const u8, spec: ?[]const u8, writer: std.fs.File.Writer) !void {
    const needle: []const u8 = spec orelse "main";
    for (result.instanced) |i| if (std.mem.eql(u8, machine, i.namespace) and std.mem.eql(u8, needle, i.name)) {
        return switch (i.value) {
            .machine => |m| graphviz.exportMachine(allocator, spec orelse i.name, m, writer, .{}),
            .scanner => |s| graphviz.exportScanner(allocator, spec orelse i.name, s, writer, .{}),
        };
    };
    if (spec) |name| {
        try std.io.getStdErr().writer().print("machine `{s}::{s}` is not defined", .{ machine, name });
        return error.InvalidUsage;
    }
    const i = result.instanced[result.instanced.len - 1];
    if (!std.mem.eql(u8, machine, i.namespace)) {
        try std.io.getStdErr().writer().print("machine `{s}` is not defined", .{machine});
        return error.InvalidUsage;
    }
    return switch (i.value) {
        .machine => |m| graphviz.exportMachine(allocator, spec orelse i.name, m, writer, .{}),
        .scanner => |s| graphviz.exportScanner(allocator, spec orelse i.name, s, writer, .{}),
    };
}

const Opts = struct {
    input: ?std.fs.File = null,
    output: ?std.fs.File = null,
    stats: bool = false,
    include_dirs: []std.fs.Dir = &.{},
    error_fmt: enum {
        gnu,
        msvc,
    } = .gnu,
    minimize: bool = true,
    graphviz: bool = false,
    graphviz_print: bool = false,
    graphviz_spec: ?[]const u8 = null,
    graphviz_machine: ?[]const u8 = null,

    fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        if (self.input) |*f| f.close();
        if (self.output) |*f| f.close();
        for (self.include_dirs) |*d| d.close();
        allocator.free(self.include_dirs);
        if (self.graphviz_spec) |v| allocator.free(v);
        if (self.graphviz_machine) |v| allocator.free(v);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // TODO: replace with FSM :)
    var opts: Opts = blk: {
        var iter = try std.process.argsWithAllocator(allocator);
        defer iter.deinit();
        var opts: Opts = .{};
        errdefer opts.deinit(allocator);
        var includes = std.ArrayListUnmanaged(std.fs.Dir){};
        errdefer {
            for (includes.items) |*d| d.close();
            includes.deinit(allocator);
        }
        _ = iter.next();
        while (iter.next()) |arg| {
            if (std.mem.startsWith(u8, arg, "--")) {
                if (std.mem.eql(u8, arg, "--help")) {
                    return usage(std.io.getStdOut().writer());
                } else if (std.mem.eql(u8, arg, "--version")) {
                    return version();
                } else if (std.mem.eql(u8, arg, "--error-format=gnu")) {
                    opts.error_fmt = .gnu;
                } else if (std.mem.eql(u8, arg, "--error-format=msvc")) {
                    opts.error_fmt = .msvc;
                } else {
                    try std.io.getStdErr().writer().writeAll("invalid argument: {s}\n");
                    try usage(std.io.getStdErr().writer());
                    return error.InvalidUsage;
                }
            } else if (std.mem.startsWith(u8, arg, "-")) {
                for (arg[1..]) |a| switch (a) {
                    'h', 'H', '?' => return usage(std.io.getStdOut().writer()),
                    'v' => return version(),
                    'o' => opts.output = try std.fs.cwd().openFile(iter.next().?, .{ .mode = .write_only }),
                    'I' => try includes.append(allocator, try std.fs.cwd().openDir(iter.next().?, .{})),
                    'V' => opts.graphviz = true,
                    'p' => opts.graphviz_print = true,
                    'S' => opts.graphviz_spec = try allocator.dupe(u8, iter.next().?),
                    'M' => opts.graphviz_machine = try allocator.dupe(u8, iter.next().?),
                    else => {
                        try std.io.getStdErr().writer().print("invalid argument: {c}\n", .{a});
                        try usage(std.io.getStdErr().writer());
                        return error.InvalidUsage;
                    },
                };
            } else {
                opts.input = try std.fs.cwd().openFile(arg, .{});
            }
        }
        opts.include_dirs = try includes.toOwnedSlice(allocator);
        break :blk opts;
    };
    defer opts.deinit(allocator);

    const input = if (opts.input) |f| f.reader().any() else std.io.getStdIn().reader().any();
    var result = try compiler.compileFromReader(allocator, input, .{});
    defer result.deinit(allocator);

    const output = if (opts.output) |f| f.writer() else std.io.getStdOut().writer();

    if (opts.graphviz) {
        if (opts.graphviz_machine) |name| {
            return graphvizExport(allocator, result, name, opts.graphviz_spec, output);
        } else if (result.instanced.len > 0) {
            const last = result.instanced[result.instanced.len - 1];
            return graphvizExport(allocator, result, last.namespace, opts.graphviz_spec, output);
        }
        return;
    }
}
