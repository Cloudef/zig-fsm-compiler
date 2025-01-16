const std = @import("std");
const common = @import("common");
const log = std.log.scoped(.mutable_machine);

const MutableMachine = @This();
pub const Runner = @import("runner.zig").Runner(MutableMachine);
pub const StateScalar = usize;
pub const EventScalar = usize;
pub const Tracing = @import("build_options").tracing;

const TraceOperations = blk: {
    if (!Tracing) break :blk void;
    const op = struct { name: []const u8, result: MutableMachine, epsilon: ?StateScalar, target: ?StateScalar };
    break :blk std.ArrayListUnmanaged(op);
};

const EventType = enum(u4) {
    any,
    nil,
    eql,
    nql,
};

const PackedTransition = packed struct {
    type: EventType,
    event: EventScalar,
    to: StateScalar,
};

const EventFormat = enum {
    auto,
    digit,
};

const EventTransition = struct {
    fmt: EventFormat = .auto,
    from: StateScalar,
    detail: PackedTransition,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.detail.type) {
            .any => return writer.print("ANY", .{}),
            .nil => return writer.print("NUL", .{}),
            .eql, .nql => {},
        }

        const scalar = switch (self.detail.type) {
            .eql => self.detail.event,
            .nql => self.detail.event,
            .any, .nil => unreachable,
        };

        switch (self.fmt) {
            .auto => {
                if (scalar >= 0 and scalar <= 255) {
                    const ch: u8 = @truncate(scalar);
                    if (std.ascii.isPrint(ch)) {
                        try writer.print("{c}", .{ch});
                    } else {
                        try writer.print("{d}", .{ch});
                    }
                } else {
                    try writer.print("{d}", .{scalar});
                }
            },
            .digit => try writer.print("{d}", .{scalar}),
        }
    }
};

states: std.DynamicBitSetUnmanaged = undefined,
final: std.DynamicBitSetUnmanaged = undefined,
events: std.ArrayListUnmanaged(EventTransition) = undefined,
num_states: StateScalar,

// debugging
operations: TraceOperations = if (Tracing) .{} else {},

pub fn init(allocator: std.mem.Allocator, num_states: StateScalar) !@This() {
    std.debug.assert(num_states > 0);
    var self: @This() = .{ .num_states = num_states };
    errdefer self.deinit(allocator);
    self.states = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_states * num_states);
    self.final = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_states);
    self.events = std.ArrayListUnmanaged(EventTransition){};
    return self;
}

pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    self.states.deinit(allocator);
    self.final.deinit(allocator);
    self.events.deinit(allocator);

    if (Tracing) {
        for (self.operations.items) |*op| op.result.deinit(allocator);
        self.operations.deinit(allocator);
    }

    self.* = undefined;
}

pub fn clone(self: @This(), allocator: std.mem.Allocator) !@This() {
    var new: @This() = .{ .num_states = self.num_states };
    errdefer new.deinit(allocator);
    new.states = try self.states.clone(allocator);
    new.final = try self.final.clone(allocator);
    new.events = try self.events.clone(allocator);

    if (Tracing) {
        new.operations = try self.operations.clone(allocator);
        for (new.operations.items) |*op| op.result = try op.result.clone(allocator);
    }

    return new;
}

pub fn resize(self: *@This(), allocator: std.mem.Allocator, num_states: StateScalar) !void {
    try self.remap(allocator, .{
        .src_num_states = self.num_states,
        .dst_num_states = num_states,
    });
}

pub fn isConnected(self: @This(), from: StateScalar, to: StateScalar) bool {
    const off = (from * self.num_states) + to;
    return self.states.isSet(off);
}

const DanglingType = enum {
    singleton,
    @"unreachable",
};

pub fn isDangling(self: @This(), state: StateScalar, kind: DanglingType) bool {
    if (state == 0) return false;
    switch (kind) {
        .singleton => {
            for (0..self.num_states) |other| {
                if (self.isConnected(other, state)) return false;
                if (self.isConnected(state, other)) return false;
            }
        },
        .@"unreachable" => {
            for (0..self.num_states) |other| if (self.isConnected(other, state)) return false;
        },
    }
    return true;
}

pub fn connect(self: *@This(), from: StateScalar, to: StateScalar) !void {
    log.debug("connect {} -> {}", .{ from, to });
    const off = (from * self.num_states) + to;
    if (self.states.isSet(off)) return error.ConnectionAlreadyDefined;
    self.states.set(off);
}

pub fn unlink(self: *@This(), from: StateScalar, to: StateScalar) !void {
    log.debug("unlink {} -> {}", .{ from, to });
    const off = (from * self.num_states) + to;
    if (!self.states.isSet(off)) return error.ConnectionNotDefined;
    self.states.unset(off);
}

const EventTransitionIterator = struct {
    fsm: *MutableMachine,
    current_state: StateScalar = 0,
    current_event: ?usize = null,

    pub fn nextFromState(self: *@This(), state: StateScalar) ?*EventTransition {
        const cevent = if (self.current_event) |event| @min(event + 1, self.fsm.events.items.len) else 0;
        for (cevent..self.fsm.events.items.len) |ev| {
            if (self.fsm.events.items[ev].from == state) {
                self.current_event = ev;
                return &self.fsm.events.items[ev];
            }
        }
        self.current_event = self.fsm.events.items.len;
        return null;
    }

    pub fn nextFromToState(self: *@This(), from: StateScalar, to: StateScalar) ?*EventTransition {
        while (self.nextFromState(from)) |res| if (res.detail.to == to) return res;
        return null;
    }

    pub fn next(self: *@This()) ?*EventTransition {
        for (self.current_state..self.fsm.num_states) |state| {
            if (self.nextFromState(state)) |res| {
                self.current_state = state;
                return res;
            }
            self.current_event = null;
        }
        self.current_state = self.fsm.num_states;
        return null;
    }

    pub fn nextToState(self: *@This(), state: StateScalar) ?*EventTransition {
        while (self.next()) |res| if (res.to == state) return res;
        return null;
    }

    pub fn reset(self: *@This()) void {
        self.current_state = 0;
        self.current_event = null;
    }
};

pub fn eventTransitionIterator(self: *const @This()) EventTransitionIterator {
    return .{ .fsm = @constCast(self) };
}

pub fn defineEventTransition(self: *@This(), allocator: std.mem.Allocator, et: EventTransition) !void {
    if (!self.isConnected(et.from, et.detail.to)) return error.ConnectionNotDefined;
    for (self.events.items) |ev| {
        if (ev.from == et.from and std.meta.eql(et.detail, ev.detail)) {
            return error.EventTransitionAlreadyDefined;
        }
    }
    try self.events.append(allocator, et);
}

pub fn connectWithEventTransition(self: *@This(), allocator: std.mem.Allocator, et: EventTransition) !void {
    try self.connect(et.from, et.detail.to);
    try self.defineEventTransition(allocator, et);
}

pub const UnlinkOption = enum {
    /// unlinks only if there are no events left from the state
    eventless,
    /// unlinks always
    force,
};

pub fn unlinkWithEventTransition(self: *@This(), t: EventTransition, option: UnlinkOption) !void {
    const from = t.from;
    const to = t.detail.to;
    var loop = true;
    while (loop) {
        loop = false;
        for (self.events.items, 0..) |*b, i| {
            if (t.from == b.from and std.meta.eql(t.detail, b.detail)) {
                _ = self.events.orderedRemove(i);
                loop = true;
                break;
            }
        }
    }
    switch (option) {
        .eventless => {
            var event_it = self.eventTransitionIterator();
            while (event_it.nextFromToState(from, to)) |_| return;
            try self.unlink(from, to);
        },
        .force => try self.unlink(from, to),
    }
}

pub const CombineOptions = struct {
    mode: enum {
        concatetate,
        @"union",
    },
};

pub fn combine(allocator: std.mem.Allocator, machines: []const @This(), options: CombineOptions) !@This() {
    if (machines.len == 1) {
        return machines[0].clone(allocator);
    }

    var num_states: StateScalar = 0;
    for (machines) |fsm| num_states += fsm.num_states;
    var offset: StateScalar = @intFromBool(options.mode == .@"union");
    num_states += offset;

    var new = try init(allocator, num_states);
    errdefer new.deinit(allocator);

    log.debug("{s} {}: {}", .{ @tagName(options.mode), machines.len, num_states });

    for (machines) |fsm| {
        var state_it = fsm.states.iterator(.{ .kind = .set, .direction = .forward });
        while (state_it.next()) |index| {
            const from = offset + index / fsm.num_states;
            const to = offset + index % fsm.num_states;
            try new.connect(from, to);
        }

        for (0..fsm.num_states) |state| {
            var event_it = fsm.eventTransitionIterator();
            while (event_it.nextFromState(state)) |t| {
                try new.defineEventTransition(allocator, .{
                    .fmt = t.fmt,
                    .from = offset + t.from,
                    .detail = .{ .type = t.detail.type, .event = t.detail.event, .to = offset + t.detail.to },
                });
            }
            if (fsm.final.isSet(state)) new.final.set(offset + state);
        }

        if (offset > 0) try new.trace(allocator, @tagName(options.mode), offset, null);

        switch (options.mode) {
            .concatetate => try new.epsilonDrawFinalsBefore(allocator, offset, offset, !fsm.final.isSet(0)),
            .@"union" => _ = try new.epsilonDrawAndRemove(allocator, 0, offset),
        }

        offset += fsm.num_states;
    }

    try new.shrinkAndValidate(allocator);
    return new;
}

pub fn applyKleeneStar(self: *@This(), allocator: std.mem.Allocator) !void {
    log.debug("kleene", .{});
    try self.epsilonDrawFinals(allocator, 0, false);
    self.final.set(0);
    try self.shrinkAndValidate(allocator);
}

pub fn applyNegation(self: *@This()) void {
    for (self.events.items) |*ev| {
        ev.detail.type = switch (ev.detail.type) {
            .any => .nil,
            .nil => .any,
            .eql => .nql,
            .nql => .eql,
        };
    }
}

pub fn zeroLength(allocator: std.mem.Allocator) !@This() {
    var self = try init(allocator, 1);
    self.final.set(0);
    return self;
}

fn lowerOrUpperScalar(comptime T: type, scalar: T) !T {
    if (T != u8 or !std.ascii.isASCII(scalar)) {
        return error.CaseInsensitiveLiteralsOnlyWorksForAsciiRightNow;
    }
    return if (std.ascii.isLower(scalar)) std.ascii.toUpper(scalar) else std.ascii.toLower(scalar);
}

pub fn fromChar(comptime T: type, allocator: std.mem.Allocator, scalar: T, ignore_case: bool) !@This() {
    var self = try init(allocator, 2);
    errdefer self.deinit(allocator);
    try self.connectWithEventTransition(allocator, .{
        .from = 0,
        .detail = .{ .type = .eql, .event = scalar, .to = 1 },
    });
    if (ignore_case) {
        const other = try lowerOrUpperScalar(T, scalar);
        if (other != scalar) {
            try self.defineEventTransition(allocator, .{
                .from = 0,
                .detail = .{ .type = .eql, .event = other, .to = 1 },
            });
        }
    }
    self.final.set(1);
    return self;
}

pub fn fromScalar(comptime T: type, allocator: std.mem.Allocator, scalar: T) !@This() {
    const self = try fromChar(T, allocator, scalar, false);
    for (self.events.items) |*ev| ev.fmt = .digit;
    return self;
}

pub fn fromSlice(comptime T: type, allocator: std.mem.Allocator, slice: []const T, ignore_case: bool) !@This() {
    if (slice.len == 0) {
        return zeroLength(allocator);
    }

    var self = try init(allocator, slice.len + 1);
    errdefer self.deinit(allocator);

    for (slice, 0..) |el, i| {
        try self.connectWithEventTransition(allocator, .{
            .from = i,
            .detail = .{ .type = .eql, .event = el, .to = i + 1 },
        });
        if (ignore_case) {
            const other = try lowerOrUpperScalar(T, el);
            if (other != el) {
                try self.defineEventTransition(allocator, .{
                    .from = i,
                    .detail = .{ .type = .eql, .event = other, .to = i + 1 },
                });
            }
        }
    }

    self.final.set(slice.len);
    return self;
}

pub fn fromUnionSlice(comptime T: type, allocator: std.mem.Allocator, slice: []const T, ignore_case: bool) !@This() {
    if (slice.len == 0) {
        return zeroLength(allocator);
    }

    std.debug.assert(std.sort.isSorted(T, slice, {}, std.sort.asc(T)));
    var self = try init(allocator, 2);
    errdefer self.deinit(allocator);

    try self.connect(0, 1);
    for (slice) |el| {
        try self.defineEventTransition(allocator, .{
            .from = 0,
            .detail = .{ .type = .eql, .event = el, .to = 1 },
        });
        if (ignore_case) {
            const other = try lowerOrUpperScalar(T, el);
            if (other != el) {
                try self.defineEventTransition(allocator, .{
                    .from = 0,
                    .detail = .{ .type = .eql, .event = other, .to = 1 },
                });
            }
        }
    }

    self.final.set(1);
    return self;
}

// --- Internal ---

fn statePropertiesAreIdentical(self: @This(), a: StateScalar, b: StateScalar) bool {
    if (a == b) return true;
    // TODO: Event transitions not checked
    return self.final.isSet(a) == self.final.isSet(b) and false;
}

fn statesAreIdentical(self: @This(), a: StateScalar, b: StateScalar) bool {
    if (a == b) return true;
    if (!self.statePropertiesAreIdentical(a, b)) return false;
    for (0..self.num_states) |to| {
        if ((to == a or to == b) and self.isConnected(a, a) != self.isConnected(b, b)) return false;
        if (self.isConnected(a, to) != self.isConnected(b, to)) return false;
    }
    return true;
}

const RemapOptions = struct {
    const Shift = union(enum) {
        none: void,
        left: struct {
            from: StateScalar,
            n: StateScalar,
        },
        right: StateScalar,
    };
    src_num_states: StateScalar,
    dst_num_states: StateScalar,
    shift: Shift = .none,
};

fn remap(self: *@This(), allocator: std.mem.Allocator, opts: RemapOptions) !void {
    if (opts.dst_num_states == opts.src_num_states and opts.shift == .none) {
        return;
    }

    log.debug("remap: {} -> {}", .{ opts.src_num_states, opts.dst_num_states });

    var new = try init(allocator, opts.dst_num_states);
    errdefer new.deinit(allocator);

    const shifter = struct {
        opts: RemapOptions,
        fn fun(this: @This(), state: StateScalar) StateScalar {
            return switch (this.opts.shift) {
                .none => state,
                .left => |s| blk: {
                    if (state < s.from) break :blk state;
                    if (s.n > state) @panic("left shift underflow");
                    break :blk state - s.n;
                },
                .right => |n| blk: {
                    if (state + n >= this.opts.dst_num_states) @panic("right shift overflow");
                    break :blk state + n;
                },
            };
        }
    }{ .opts = opts };

    const shortest_states = if (opts.shift != .left) @min(opts.src_num_states, opts.dst_num_states) else opts.src_num_states;
    for (0..shortest_states) |from| {
        for (0..shortest_states) |to| {
            const src_off = from * opts.src_num_states + to;
            const dst_off = shifter.fun(from) * opts.dst_num_states + shifter.fun(to);
            if (self.states.isSet(src_off)) new.states.set(dst_off);
        }
        if (self.final.isSet(from)) new.final.set(shifter.fun(from));
    }

    var event_it = self.eventTransitionIterator();
    while (event_it.next()) |t| {
        try new.defineEventTransition(allocator, .{
            .fmt = t.fmt,
            .from = shifter.fun(t.from),
            .detail = .{
                .type = t.detail.type,
                .event = t.detail.event,
                .to = shifter.fun(t.detail.to),
            },
        });
    }

    if (Tracing) {
        new.operations = self.operations;
        self.operations = .{};
    }

    self.deinit(allocator);
    self.* = new;
}

fn shift(self: *@This(), allocator: std.mem.Allocator, opts: RemapOptions.Shift) !void {
    const dst_num_states = switch (opts) {
        .left => |s| self.num_states - s.n,
        .right => |n| self.num_states + n,
        .none => return error.MakesNoSense,
    };
    try self.remap(allocator, .{
        .src_num_states = self.num_states,
        .dst_num_states = dst_num_states,
        .shift = opts,
    });
}

fn addState(self: *@This(), allocator: std.mem.Allocator) !StateScalar {
    for (0..self.num_states) |state| if (self.isDangling(state, .singleton)) return state;
    const new_state = self.num_states;
    try self.resize(allocator, self.num_states + 1);
    return new_state;
}

fn removeState(self: *@This(), allocator: std.mem.Allocator, state: StateScalar) void {
    log.debug("remove {}", .{state});
    self.trace(allocator, "remove", state, null) catch unreachable;

    var state_it = self.states.iterator(.{ .kind = .set, .direction = .forward });
    while (state_it.next()) |index| {
        const from = index / self.num_states;
        if (from == state) self.states.unset(index);
    }
    self.final.unset(state);

    var loop = true;
    while (loop) {
        loop = false;
        for (self.events.items, 0..) |*t, i| {
            if (t.from == state) {
                _ = self.events.orderedRemove(i);
                loop = true;
                break;
            }
        }
    }
}

fn inheritState(self: *@This(), allocator: std.mem.Allocator, src: StateScalar, dst: StateScalar) !void {
    log.debug("copy {} -> {}", .{ src, dst });
    if (self.final.isSet(src)) self.final.set(dst);

    var state_it = self.states.iterator(.{ .kind = .set, .direction = .forward });
    while (state_it.next()) |index| {
        const from = index / self.num_states;
        const to = index % self.num_states;
        if (from == src) self.connect(dst, to) catch {};
    }

    var event_it = self.eventTransitionIterator();
    while (event_it.nextFromState(src)) |t| {
        if (t.detail.to == t.from) {
            self.unlinkWithEventTransition(t.*, .eventless) catch unreachable;
            self.connect(dst, dst) catch {};
        }
        try self.defineEventTransition(allocator, .{
            .fmt = t.fmt,
            .from = dst,
            .detail = .{
                .type = t.detail.type,
                .event = t.detail.event,
                .to = if (t.detail.to == t.from) dst else t.detail.to,
            },
        });
    }
}

fn epsilonDraw(self: *@This(), allocator: std.mem.Allocator, target: StateScalar, epsilon: StateScalar) !bool {
    var eps_ev_it = self.eventTransitionIterator();
    var tgt_ev_it = self.eventTransitionIterator();
    while (eps_ev_it.nextFromState(epsilon)) |a| {
        while (tgt_ev_it.nextFromState(target)) |b| {
            if (std.meta.eql(a.detail, b.detail)) {
                log.debug("{} -> {}: same event transition, combination required", .{ target, epsilon });
                const new_state = try self.addState(allocator);
                var drawn: u8 = 0;
                if (try self.epsilonDrawAndRemove(allocator, new_state, a.detail.to)) {
                    try self.unlinkWithEventTransition(a.*, .eventless);
                    drawn += 1;
                }
                if (try self.epsilonDrawAndRemove(allocator, new_state, b.detail.to)) {
                    try self.unlinkWithEventTransition(b.*, .eventless);
                    drawn += 1;
                }
                if (drawn == 2) {
                    try self.connectWithEventTransition(allocator, .{
                        .fmt = a.fmt,
                        .from = a.from,
                        .detail = .{ .type = a.detail.type, .event = a.detail.event, .to = new_state },
                    });
                }
                const has_events = blk: {
                    var iter = self.eventTransitionIterator();
                    while (iter.nextFromState(epsilon)) |_| break :blk true;
                    break :blk false;
                };
                if (!has_events) {
                    log.debug("skipping draw: {} -> {} (epsilon turned dangling)", .{ target, epsilon });
                    return false;
                }
            }
        }
    }

    if (!self.statePropertiesAreIdentical(target, epsilon)) {
        log.debug("draw: {} -> {}", .{ target, epsilon });
        try self.trace(allocator, "draw", epsilon, target);
        try self.inheritState(allocator, epsilon, target);
        return true;
    } else {
        log.debug("skipping draw: {} -> {} (identical properties)", .{ target, epsilon });
        return false;
    }
}

fn epsilonDrawAndRemove(self: *@This(), allocator: std.mem.Allocator, target: StateScalar, epsilon: StateScalar) anyerror!bool {
    const ret = try self.epsilonDraw(allocator, target, epsilon);
    if (self.isDangling(epsilon, .@"unreachable")) self.removeState(allocator, epsilon);
    return ret;
}

fn epsilonDrawFinalsBefore(self: *@This(), allocator: std.mem.Allocator, epsilon: StateScalar, before: StateScalar, lose_final_state: bool) !void {
    var final_it = self.final.iterator(.{ .kind = .set, .direction = .forward });
    while (final_it.next()) |state| {
        if (state >= before) continue;
        _ = try self.epsilonDraw(allocator, state, epsilon);
        if (lose_final_state) self.final.unset(state);
    }
    if (self.isDangling(epsilon, .@"unreachable")) self.removeState(allocator, epsilon);
}

fn epsilonDrawFinals(self: *@This(), allocator: std.mem.Allocator, epsilon: StateScalar, lose_final_state: bool) !void {
    try self.epsilonDrawFinalsBefore(allocator, epsilon, self.num_states, lose_final_state);
}

fn forwardConnections(self: *@This(), src: StateScalar, dst: StateScalar) void {
    for (0..self.num_states) |child| {
        if (self.isConnected(child, src)) {
            self.unlink(child, src) catch unreachable;
            self.connect(child, dst) catch {};
            var event_it = self.eventTransitionIterator();
            while (event_it.nextFromToState(child, src)) |t| t.detail.to = dst;
        }
    }
}

fn shrinkAndValidate(self: *@This(), allocator: std.mem.Allocator) !void {
    std.debug.assert(self.num_states > 0);
    try self.trace(allocator, "before shrink", null, null);
    const old_num_states = self.num_states;

    while (true) {
        var pruned = false;
        for (0..self.num_states) |state| {
            if (self.isDangling(state, .@"unreachable")) {
                log.debug("prune dangling: {}", .{state});
                try self.shift(allocator, .{ .left = .{ .from = state, .n = 1 } });
                pruned = true;
                break;
            }
        }
        if (!pruned) break;
    }

    while (true) outer: {
        var combined = false;
        for (0..self.num_states) |a| {
            for (0..self.num_states) |b| if (a != b and self.statesAreIdentical(a, b)) {
                const min = @min(a, b);
                const max = @max(a, b);
                log.debug("combine: {} -> {}", .{ max, min });
                try self.trace(allocator, "combine", min, max);
                self.forwardConnections(max, min);
                try self.shift(allocator, .{ .left = .{ .from = max, .n = 1 } });
                combined = true;
                break :outer;
            };
        }
        if (!combined) break;
    }

    log.debug("after shrink: {} -> {}", .{ old_num_states, self.num_states });

    if (self.final.count() > 0) {
        var final_it = self.final.iterator(.{ .kind = .set, .direction = .forward });
        while (final_it.next()) |state| if (state >= self.num_states) {
            return error.ValidationFailure;
        };
    } else {
        return error.ValidationFailure;
    }
}

fn trace(self: *@This(), allocator: std.mem.Allocator, name: []const u8, epsilon: ?StateScalar, target: ?StateScalar) !void {
    if (Tracing) {
        try self.operations.append(allocator, .{
            .name = name,
            .result = try self.clone(allocator),
            .epsilon = epsilon,
            .target = target,
        });
    }
}
