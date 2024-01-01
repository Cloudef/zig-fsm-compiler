const std = @import("std");
const compiler = @import("fsm-compiler");
const machine = compiler.machine;
const scanner = compiler.scanner;

pub const ExportOptions = struct {
    state_shape: []const u8 = "circle",
    final_state_shape: []const u8 = "doublecircle",
};

fn exportMachineInnerInner(_: std.mem.Allocator, fsm: machine.Mutable, writer: anytype, offset: usize, options: ExportOptions) !void {
    try writer.print("node[shape=point];in_{};\n", .{offset});

    if (fsm.meta.bs.final.count() > 0) {
        try writer.print("node[shape={s} fixedsize=false];", .{options.final_state_shape});
        var final_it = fsm.meta.bs.final.iterator(.{ .kind = .set, .direction = .forward });
        var needs_space = false;
        while (final_it.next()) |state| if (!fsm.isDangling(state, .singleton)) {
            try writer.print("{s}{}", .{ if (needs_space) " " else "", offset + state });
            needs_space = true;
        };
        try writer.print(";\n", .{});
    }

    try writer.print("node[shape={s} fixedsize=false];\n", .{options.state_shape});
    try writer.print("in_{}->{}[label=IN];\n", .{offset, offset});

    if (offset > 0) {
        for (0..fsm.num_states) |state| if (!fsm.isDangling(state, .singleton)) {
            try writer.print("{}[label={}];\n", .{offset + state, state});
        };
    }

    var state_it = fsm.states.iterator(.{ .kind = .set, .direction = .forward });
    while (state_it.next()) |index| {
        const from = index / fsm.num_states;
        const to = index % fsm.num_states;
        try writer.print("{}->{}", .{ offset + from, offset + to });

        var name = std.BoundedArray(u8, 128){};
        var iter = fsm.eventTransitionIterator();
        while (iter.nextFromToState(from, to)) |t| try name.append(@truncate(t.event));
        const is_union = name.len > 1;

        // TODO: make proper event formatter
        if (std.mem.eql(u8, name.constSlice(), "abcdefghijklmnopqrstuvwxyz")) {
            try name.resize(0);
            try name.appendSlice("a-z");
        } else if (std.mem.eql(u8, name.constSlice(), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
            try name.resize(0);
            try name.appendSlice("A-Z");
        } else if (std.mem.eql(u8, name.constSlice(), "\n")) {
            try name.resize(0);
            try name.appendSlice("\\\\n");
        }

        const is_print = blk: {
            if (fsm.meta.bs.scalar.isSet(from)) break :blk false;
            for (name.constSlice()) |ch| if (!std.ascii.isPrint(ch)) break :blk false;
            break :blk true;
        };

        // TODO: make proper event formatter
        if (is_print) {
            if (is_union) {
                if (fsm.meta.bs.negation.isSet(from)) try name.insert(0, '^');
                try writer.print("[label=\"[{s}]\"]", .{name.constSlice()});
            } else if (name.len > 0) {
                try name.insert(0, '\'');
                try name.append('\'');
                if (fsm.meta.bs.negation.isSet(from)) try name.insert(0, '!');
                try writer.print("[label=\"{s}\"]", .{name.constSlice()});
            }
        } else {
            if (fsm.meta.bs.negation.isSet(from)) try name.insert(0, '!');
            try writer.print("[label=\"{d}\"]", .{name.constSlice()});
        }
        try writer.print(";\n", .{});
    }
}

fn exportMachineInner(allocator: std.mem.Allocator, title: []const u8, fsm: machine.Mutable, writer: anytype, initial_offset: usize, options: ExportOptions) !usize {
    var offset: usize = initial_offset;
    try writer.print("subgraph cluster_{}{{\n", .{offset});
    if (machine.Mutable.Tracing) {
        try writer.print("label=<{}. result [ {s} ]>;\n", .{fsm.operations.items.len + 1, fsm.name});
    } else {
        if (std.mem.count(u8, title, "\"") == 0) {
            try writer.print("label=\"{s}\";\n", .{title});
        } else if (std.mem.count(u8, title, "<") == 0 and std.mem.count(u8, title, ">") == 0) {
            try writer.print("label=<{s}>;\n", .{title});
        } else {
            return error.InvalidMachineName;
        }
    }
    try exportMachineInnerInner(allocator, fsm, writer, offset, options);
    try writer.print("{}->{}[style=invis constraint=false];\n", .{offset, offset + fsm.num_states - 1});
    try writer.print("}}\n", .{});
    offset += fsm.num_states;
    if (machine.Mutable.Tracing) {
        for (0..fsm.operations.items.len) |index| {
            const op = fsm.operations.items[(fsm.operations.items.len - 1) - index];
            try writer.print("subgraph cluster_{}{{\n", .{offset});
            try writer.print("label=\"{}. {s}\";\n", .{fsm.operations.items.len - index, op.name});
            if (op.epsilon) |epsilon| {
                try writer.print("node[color=red];{};\n", .{offset + epsilon});
                try writer.print("{}[label={}];\n", .{offset + epsilon, epsilon});
            }
            if (op.target) |target| {
                try writer.print("node[color=blue];{};\n", .{offset + target});
                try writer.print("{}[label={}];\n", .{offset + target, target});
                try writer.print("{}->{}[style=dashed];\n", .{offset + target, offset + op.epsilon.?});
            }
            try writer.print("node[color=black];\n", .{});
            try exportMachineInnerInner(allocator, op.result, writer, offset, options);
            try writer.print("{}->{}[style=invis constraint=false];\n", .{offset, offset + op.result.num_states - 1});
            try writer.print("}}\n", .{});
            offset += op.result.num_states;
        }
    }
    return offset;
}

pub fn exportMachines(allocator: std.mem.Allocator, title: []const u8, machines: []const machine.Mutable, writer: anytype, options: ExportOptions) !void {
    try writer.print("digraph{{\n", .{});
    try writer.print("fontname=\"Helvetica,Arial,sans-serif\";\n", .{});
    try writer.print("node[fontname=\"Helvetica,Arial,sans-serif\"];\n", .{});
    try writer.print("edge[fontname=\"Helvetica,Arial,sans-serif\"];\n", .{});
    try writer.print("rankdir=LR;\n", .{});
    try writer.print("labelloc=\"t\";\n", .{});
    var offset: usize = 0;
    for (machines) |m| offset = try exportMachineInner(allocator, title, m, writer, offset, options);
    try writer.print("}}\n", .{});
}

pub fn exportMachine(allocator: std.mem.Allocator, title: []const u8, fsm: machine.Mutable, writer: anytype, options: ExportOptions) !void {
    try exportMachines(allocator, title, &.{fsm}, writer, options);
}

pub fn exportScanner(allocator: std.mem.Allocator, title: []const u8, scan: scanner.Mutable, writer: anytype, options: ExportOptions) !void {
    try exportMachines(allocator, title, scan.machines.items, writer, options);
}
