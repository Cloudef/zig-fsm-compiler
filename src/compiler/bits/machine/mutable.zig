const std = @import("std");
const common = @import("common");
const log = std.log.scoped(.mutable_machine);

pub fn Machine(comptime StateType: type, comptime EventTypeDef: type) type {
    const EventTransitionHash = std.math.IntFittingRange(0, std.math.maxInt(usize) * 2);

    const Meta = common.MultiDynamicBitSetUnmanaged(struct {
        final: std.DynamicBitSetUnmanaged,
        negation: std.DynamicBitSetUnmanaged,
        scalar: std.DynamicBitSetUnmanaged,
    });

    return struct {
        const MutableMachine = @This();
        pub const Runner = @import("runner.zig").Runner(MutableMachine);
        pub const EventType = EventTypeDef;
        pub const Tracing = @import("build_options").tracing;

        const TraceOperations = blk: {
            if (!Tracing) break :blk void;
            const op = struct { name: []const u8, result: MutableMachine, epsilon: ?StateType, target: ?StateType };
            break :blk std.ArrayListUnmanaged(op);
        };

        states: std.DynamicBitSetUnmanaged = undefined,
        meta: Meta = undefined,
        events: []?StateType = undefined,
        num_states: StateType,
        event_min: EventType,
        event_max: EventType,

        // debugging
        operations: TraceOperations = if (Tracing) .{} else {},

        pub fn init(allocator: std.mem.Allocator, num_states: StateType, event_min: EventType, event_max: EventType) !@This() {
            std.debug.assert(num_states > 0);
            const num_events = (event_max - event_min) + 1;
            var self: @This() = .{ .num_states = num_states, .event_min = event_min, .event_max = event_max };
            errdefer self.deinit(allocator);
            self.states = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_states * num_states);
            self.meta = try Meta.init(allocator, num_states);
            self.events = try allocator.alloc(?StateType, num_states * num_events);
            @memset(self.events, null);
            return self;
        }

        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            self.states.deinit(allocator);
            self.meta.deinit(allocator);
            allocator.free(self.events);

            if (Tracing) {
                for (self.operations.items) |*op| op.result.deinit(allocator);
                self.operations.deinit(allocator);
            }

            self.* = undefined;
        }

        pub fn clone(self: @This(), allocator: std.mem.Allocator) !@This() {
            var new: @This() = .{ .num_states = self.num_states, .event_min = self.event_min, .event_max = self.event_max };
            errdefer new.deinit(allocator);
            new.states = try self.states.clone(allocator);
            new.meta = try self.meta.clone(allocator);
            new.events = try allocator.dupe(?StateType, self.events);

            if (Tracing) {
                new.operations = try self.operations.clone(allocator);
                for (new.operations.items) |*op| op.result = try op.result.clone(allocator );
            }

            return new;
        }

        pub fn resize(self: *@This(), allocator: std.mem.Allocator, num_states: StateType, event_min: EventType, event_max: EventType) !void {
            try self.remap(allocator, .{
                .src_num_states = self.num_states,
                .dst_num_states = num_states,
                .src_min_event = self.event_min,
                .src_max_event = self.event_max,
                .dst_min_event = event_min,
                .dst_max_event = event_max,
            });
        }

        pub fn isConnected(self: @This(), from: StateType, to: StateType) bool {
            const off = (from * self.num_states) + to;
            return self.states.isSet(off);
        }

        const DanglingType = enum {
            singleton,
            @"unreachable",
        };

        pub fn isDangling(self: @This(), state: StateType, kind: DanglingType) bool {
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

        pub fn connect(self: *@This(), from: StateType, to: StateType) !void {
            log.debug("connect {} -> {}", .{from, to});
            const off = (from * self.num_states) + to;
            if (self.states.isSet(off)) return error.ConnectionAlreadyDefined;
            self.states.set(off);
        }

        pub fn unlink(self: *@This(), from: StateType, to: StateType) !void {
            log.debug("unlink {} -> {}", .{from, to});
            const off = (from * self.num_states) + to;
            if (!self.states.isSet(off)) return error.ConnectionNotDefined;
            self.states.unset(off);
        }

        const EventTransitionIterator = struct {
            const Transition = struct {
                from: StateType,
                to: StateType,
                event: EventType,
            };

            fsm: *const MutableMachine,
            current_state: StateType = 0,
            current_event: ?EventType = null,

            pub fn nextFromState(self: *@This(), state: StateType) ?Transition {
                const num_events = (self.fsm.event_max - self.fsm.event_min) + 1;
                const cevent = if (self.current_event) |event| @min(event + 1, num_events) else 0;
                for (cevent..num_events) |ev| if (self.fsm.events[state * num_events + ev]) |ev_to| {
                    self.current_event = ev;
                    return .{ .from = state, .to = ev_to, .event = self.fsm.event_min + ev };
                };
                self.current_event = num_events;
                return null;
            }

            pub fn nextFromToState(self: *@This(), from: StateType, to: StateType) ?Transition {
                while (self.nextFromState(from)) |res| if (res.to == to) return res;
                return null;
            }

            pub fn next(self: *@This()) ?Transition {
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

            pub fn nextToState(self: *@This(), state: StateType) ?Transition {
                while (self.next()) |res| if (res.to == state) return res;
                return null;
            }

            pub fn reset(self: *@This()) void {
                self.current_state = 0;
                self.current_event = null;
            }
        };

        pub fn eventTransitionIterator(self: *const @This()) EventTransitionIterator {
            return .{ .fsm = self };
        }

        pub fn defineEventTransition(self: *@This(), event: EventType, from: StateType, to: StateType) !void {
            if (event <= std.math.maxInt(u8) and event >= std.math.minInt(u8) and std.ascii.isPrint(@truncate(event))) {
                log.debug("event {c}, {} -> {}", .{@as(u8, @truncate(event)), from, to});
            } else {
                log.debug("event {d}, {} -> {}", .{event, from, to});
            }
            if (!self.isConnected(from, to)) return error.ConnectionNotDefined;
            const num_events = (self.event_max - self.event_min) + 1;
            const ev_off = from * num_events + (event - self.event_min);
            if (self.events[ev_off] != null) return error.EventTransitionAlreadyDefined;
            self.events[ev_off] = to;
        }

        pub fn removeEventTransition(self: *@This(), event: EventType, from: StateType) !void {
            const num_events = (self.event_max - self.event_min) + 1;
            const ev_off = from * num_events + (event - self.event_min);
            if (self.events[ev_off] == null) return error.EventTransitionNotDefined;
            self.events[ev_off] = null;
        }

        pub fn connectWithEventTransition(self: *@This(), event: EventType, from: StateType, to: StateType) !void {
            try self.connect(from, to);
            try self.defineEventTransition(event, from, to);
        }

        pub fn destinationForEvent(self: @This(), event: EventType, from: StateType) !?StateType {
            if (event < self.event_min or event > self.event_max) return error.InvalidEvent;
            const ev_off = (from * self.num_states) + (event - self.event_min);
            return self.events[ev_off];
        }

        pub const UnlinkOption = enum {
            /// unlinks only if there are no events left from the state
            eventless,
            /// unlinks always
            force,
        };

        pub fn unlinkWithEventTransition(self: *@This(), event: EventType, from: StateType, to: StateType, option: UnlinkOption) !void {
            try self.removeEventTransition(event, from);
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
            if (machines.len == 1) return machines[0].clone(allocator);

            var num_states: StateType = 0;
            var event_min: EventType = std.math.maxInt(EventType);
            var event_max: EventType = std.math.minInt(EventType);

            for (machines) |fsm| {
                num_states += fsm.num_states;
                if (fsm.num_states > 0) {
                    event_min = @min(event_min, fsm.event_min);
                    event_max = @max(event_max, fsm.event_max);
                }
            }

            var offset: StateType = @intFromBool(options.mode == .@"union");
            num_states += offset;

            var new = try init(allocator, num_states, event_min, event_max);
            errdefer new.deinit(allocator);

            log.debug("{s} {}: {}, {}, {}", .{@tagName(options.mode), machines.len, num_states, event_min, event_max});

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
                        try new.defineEventTransition(t.event, offset + t.from, offset + t.to);
                    }
                    new.meta.inheritFromOther(fsm.meta, state, offset + state);
                }

                if (offset > 0) try new.trace(allocator, @tagName(options.mode), offset, null);

                switch (options.mode) {
                    .concatetate => try new.epsilonDrawFinalsBefore(allocator, offset, offset, !fsm.meta.bs.final.isSet(0)),
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
            self.meta.bs.final.set(0);
            try self.shrinkAndValidate(allocator);
        }

        pub fn applyNegation(self: *@This()) void {
            self.meta.bs.negation.setAll();
        }

        pub fn zeroLength(allocator: std.mem.Allocator) !@This() {
            var self = try init(allocator, 1, 0, 0);
            self.meta.bs.final.set(0);
            return self;
        }

        fn lowerOrUpperScalar(comptime T: type, scalar: T) !T {
            if (T != u8 or !std.ascii.isASCII(scalar)) {
                return error.CaseInsensitiveLiteralsOnlyWorksForAsciiRightNow;
            }
            return if (std.ascii.isLower(scalar)) std.ascii.toUpper(scalar) else std.ascii.toLower(scalar);
        }

        fn minMaxScalar(comptime T: type, min: T, max: T, scalar: T, ignore_case: bool) !std.meta.Tuple(&.{ T, T }) {
            if (ignore_case) {
                const scalar2 = try lowerOrUpperScalar(T, scalar);
                return .{ @min(scalar2, @min(scalar, min)), @max(scalar2, @max(scalar, max)) };
            }
            return .{ scalar, scalar };
        }

        pub fn fromChar(comptime T: type, allocator: std.mem.Allocator, scalar: T, ignore_case: bool) !@This() {
            const range = try minMaxScalar(T, scalar, scalar, scalar, ignore_case);
            var self = try init(allocator, 2, range.@"0", range.@"1");
            errdefer self.deinit(allocator);
            try self.connectWithEventTransition(scalar, 0, 1);
            if (ignore_case) self.defineEventTransition(try lowerOrUpperScalar(T, scalar), 0, 1) catch {};
            self.meta.bs.final.set(1);
            return self;
        }

        pub fn fromScalar(comptime T: type, allocator: std.mem.Allocator, scalar: T) !@This() {
            var self = try fromChar(T, allocator, scalar, false);
            self.meta.bs.scalar.setAll();
            return self;
        }

        fn minMaxSlice(comptime T: type, slice: []const T, ignore_case: bool) !std.meta.Tuple(&.{ T, T }) {
            if (ignore_case) {
                var range: std.meta.Tuple(&.{ T, T }) = .{ std.math.maxInt(T), std.math.minInt(T) };
                for (slice) |el| range = try minMaxScalar(T, range.@"0", range.@"1", el, true);
                return range;
            }
            return std.mem.minMax(T, slice);
        }

        pub fn fromSlice(comptime T: type, allocator: std.mem.Allocator, slice: []const T, ignore_case: bool) !@This() {
            if (slice.len == 0) {
                return zeroLength(allocator);
            }

            const range = try minMaxSlice(T, slice, ignore_case);
            var self = try init(allocator, slice.len + 1, range.@"0", range.@"1");
            errdefer self.deinit(allocator);

            for (slice, 0..) |el, i| {
                try self.connectWithEventTransition(el, i, i + 1);
                if (ignore_case) self.defineEventTransition(try lowerOrUpperScalar(u8, el), i, i + 1) catch {};
            }

            self.meta.bs.final.set(slice.len);
            return self;
        }

        pub fn fromUnionSlice(comptime T: type, allocator: std.mem.Allocator, slice: []const T, ignore_case: bool) !@This() {
            if (slice.len == 0) {
                return zeroLength(allocator);
            }

            std.debug.assert(std.sort.isSorted(T, slice, {}, std.sort.asc(T)));
            const range = try minMaxSlice(T, slice, ignore_case);
            var self = try init(allocator, 2, range.@"0", range.@"1");
            errdefer self.deinit(allocator);

            try self.connect(0, 1);
            for (slice) |el| {
                try self.defineEventTransition(el, 0, 1);
                if (ignore_case) self.defineEventTransition(try lowerOrUpperScalar(u8, el), 0, 1) catch {};
            }

            self.meta.bs.final.set(1);
            return self;
        }

        // --- Internal ---

        fn eventTransitionHash(self: @This(), state: StateType) EventTransitionHash {
            var hash: EventTransitionHash = 0;
            var iter = self.eventTransitionIterator();
            const num_events = (self.event_max - self.event_min) + 1;
            while (iter.nextFromState(state)) |t| {
                if (t.to == t.from) hash += t.event + num_events * self.num_states
                else hash += t.event + num_events * t.to;
            }
            return hash;
        }

        fn statePropertiesAreIdentical(self: @This(), a: StateType, b: StateType) bool {
            if (a == b) return true;
            return self.meta.eql(a, b) and self.eventTransitionHash(a) == self.eventTransitionHash(b);
        }

        fn statesAreIdentical(self: @This(), a: StateType, b: StateType) bool {
            if (a == b) return true;
            if (!self.statePropertiesAreIdentical(a, b)) return false;
            for (0..self.num_states) |to| {
                if ((to == a or to == b) and self.isConnected(a, a) != self.isConnected(b, b)) return false;
                if (self.isConnected(a, to) != self.isConnected(b, to)) return false;
            }
            return true;
        }

        const RemapOptions = struct {
            const Shift = union (enum) {
                none: void,
                left: struct {
                    from: StateType,
                    n: StateType,
                },
                right: StateType,
            };
            src_num_states: StateType,
            dst_num_states: StateType,
            src_min_event: EventType,
            dst_min_event: EventType,
            src_max_event: EventType,
            dst_max_event: EventType,
            shift: Shift = .none,
        };

        fn remap(self: *@This(), allocator: std.mem.Allocator, opts: RemapOptions) !void {
            const src_num_events = (opts.src_max_event - opts.src_min_event) + 1;
            const dst_num_events = (opts.dst_max_event - opts.dst_min_event) + 1;

            if (opts.dst_num_states == opts.src_num_states and src_num_events == dst_num_events and opts.shift == .none) {
                return;
            }

            log.debug("remap: {} -> {}, {} -> {}", .{opts.src_num_states, opts.dst_num_states, src_num_events, dst_num_events});

            var new = try init(allocator, opts.dst_num_states, opts.dst_min_event, opts.dst_max_event);
            errdefer new.deinit(allocator);

            const shifter = struct {
                opts: RemapOptions,
                fn fun(this: @This(), state: StateType) StateType {
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
                        }
                    };
                }
            }{ .opts = opts };

            const shortest_events = @min(src_num_events, dst_num_events);
            const shortest_states = if (opts.shift == .none) @min(opts.src_num_states, opts.dst_num_states)
                                    else opts.src_num_states;
            for (0..shortest_states) |from| {
                for (0..shortest_states) |to| {
                    const src_off = from * opts.src_num_states + to;
                    const dst_off = shifter.fun(from) * opts.dst_num_states + shifter.fun(to);
                    if (self.states.isSet(src_off)) new.states.set(dst_off);
                }

                new.meta.inheritFromOther(self.meta, from, shifter.fun(from));

                for (0..shortest_events) |ev| {
                    const src_ev_off = from * src_num_events + ev;
                    const dst_ev_off = shifter.fun(from) * dst_num_events + ((opts.src_min_event + ev) - opts.dst_min_event);
                    if (self.events[src_ev_off]) |ev_to| new.events[dst_ev_off] = shifter.fun(ev_to);
                }
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
                .src_min_event = self.event_min,
                .src_max_event = self.event_max,
                .dst_min_event = self.event_min,
                .dst_max_event = self.event_max,
                .shift = opts,
            });
        }

        fn addState(self: *@This(), allocator: std.mem.Allocator) !StateType {
            for (0..self.num_states) |state| if (self.isDangling(state, .singleton)) return state;
            const new_state = self.num_states;
            try self.resize(allocator, self.num_states + 1, self.event_min, self.event_max);
            return new_state;
        }

        fn removeState(self: *@This(), allocator: std.mem.Allocator, state: StateType) void {
            log.debug("remove {}", .{state});
            self.trace(allocator, "remove", state, null) catch unreachable;
            var state_it = self.states.iterator(.{ .kind = .set, .direction = .forward });
            while (state_it.next()) |index| {
                const from = index / self.num_states;
                const to = index % self.num_states;
                if (from == state) {
                    self.states.unset(index);
                    var iter = self.eventTransitionIterator();
                    while (iter.nextFromToState(from, to)) |t| {
                        self.removeEventTransition(t.event, t.from) catch unreachable;
                    }
                }
            }
            self.meta.clear(state);
        }

        fn inheritState(self: *@This(), src: StateType, dst: StateType) void {
            log.debug("copy {} -> {}", .{src, dst});
            self.meta.inherit(src, dst);

            const num_events = (self.event_max - self.event_min) + 1;
            var event_it = self.eventTransitionIterator();
            while (event_it.nextFromState(src)) |t| {
                const ev_off = dst * num_events + (t.event - self.event_min);
                if (self.events[ev_off] == null) {
                    self.events[ev_off] = blk: {
                        if (t.to == t.from) {
                            self.unlinkWithEventTransition(t.event, t.from, t.to, .eventless) catch unreachable;
                            self.connect(dst, dst) catch {};
                            break :blk dst;
                        }
                        break :blk t.to;
                    };
                }
            }

            var state_it = self.states.iterator(.{ .kind = .set, .direction = .forward });
            while (state_it.next()) |index| {
                const from = index / self.num_states;
                const to = index % self.num_states;
                if (from == src) self.connect(dst, to) catch {};
            }
        }

        fn epsilonDraw(self: *@This(), allocator: std.mem.Allocator, target: StateType, epsilon: StateType) !bool {
            var eps_ev_it = self.eventTransitionIterator();
            var tgt_ev_it = self.eventTransitionIterator();
            while (eps_ev_it.nextFromState(epsilon)) |a| {
                while (tgt_ev_it.nextFromState(target)) |b| {
                    if (a.event == b.event and a.to != b.to) {
                        log.debug("{} -> {}: same event transition, combination required", .{target, epsilon});
                        const new_state = try self.addState(allocator);
                        var drawn: u8 = 0;
                        if (try self.epsilonDrawAndRemove(allocator, new_state, a.to)) {
                            try self.unlinkWithEventTransition(a.event, a.from, a.to, .eventless);
                            drawn += 1;
                        }
                        if (try self.epsilonDrawAndRemove(allocator, new_state, b.to)) {
                            try self.unlinkWithEventTransition(a.event, b.from, b.to, .eventless);
                            drawn += 1;
                        }
                        if (drawn == 2) {
                            try self.connectWithEventTransition(a.event, a.from, new_state);
                        }
                        if (self.eventTransitionHash(epsilon) == 0) {
                            log.debug("skipping draw: {} -> {} (epsilon turned dangling)", .{target, epsilon});
                            return false;
                        }
                    }
                }
            }

            if (!self.statePropertiesAreIdentical(target, epsilon)) {
                log.debug("draw: {} -> {}", .{target, epsilon});
                try self.trace(allocator, "draw", epsilon, target);
                self.inheritState(epsilon, target);
                return true;
            } else {
                log.debug("skipping draw: {} -> {} (identical properties)", .{target, epsilon});
                return false;
            }
        }

        fn epsilonDrawAndRemove(self: *@This(), allocator: std.mem.Allocator, target: StateType, epsilon: StateType) anyerror!bool {
            const ret = try self.epsilonDraw(allocator, target, epsilon);
            if (self.isDangling(epsilon, .@"unreachable")) self.removeState(allocator, epsilon);
            return ret;
        }

        fn epsilonDrawFinalsBefore(self: *@This(), allocator: std.mem.Allocator, epsilon: StateType, before: StateType, lose_final_state: bool) !void {
            var final_it = self.meta.bs.final.iterator(.{ .kind = .set, .direction = .forward });
            while (final_it.next()) |state| {
                if (state >= before) continue;
                _ = try self.epsilonDraw(allocator, state, epsilon);
                if (lose_final_state) self.meta.bs.final.unset(state);
            }
            if (self.isDangling(epsilon, .@"unreachable")) self.removeState(allocator, epsilon);
        }

        fn epsilonDrawFinals(self: *@This(), allocator: std.mem.Allocator, epsilon: StateType, lose_final_state: bool) !void {
            try self.epsilonDrawFinalsBefore(allocator, epsilon, self.num_states, lose_final_state);
        }

        fn forwardConnections(self: *@This(), src: StateType, dst: StateType) void {
            for (0..self.num_states) |child| {
                if (self.isConnected(child, src)) {
                    self.unlink(child, src) catch unreachable;
                    self.connect(child, dst) catch {};
                    var event_it = self.eventTransitionIterator();
                    while (event_it.nextFromToState(child, src)) |t| {
                        self.removeEventTransition(t.event, t.from) catch unreachable;
                        self.defineEventTransition(t.event, t.from, dst) catch {};
                    }
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
                        try self.shift(allocator, .{ .left = .{ .from = state, .n = 1 }});
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
                        log.debug("combine: {} -> {}", .{max, min});
                        try self.trace(allocator, "combine", min, max);
                        self.forwardConnections(max, min);
                        try self.shift(allocator, .{ .left = .{ .from = max, .n = 1 }});
                        combined = true;
                        break :outer;
                    };
                }
                if (!combined) break;
            }

            log.debug("after shrink: {} -> {}", .{old_num_states, self.num_states});

            if (self.meta.bs.final.count() > 0) {
                var final_it = self.meta.bs.final.iterator(.{ .kind = .set, .direction = .forward });
                while (final_it.next()) |state| if (state >= self.num_states) {
                    return error.ValidationFailure;
                };
            } else {
                return error.ValidationFailure;
            }
        }

        fn trace(self: *@This(), allocator: std.mem.Allocator, name: []const u8, epsilon: ?StateType, target: ?StateType) !void {
            if (Tracing) {
                try self.operations.append(allocator, .{
                    .name = name,
                    .result = try self.clone(allocator),
                    .epsilon = epsilon,
                    .target = target,
                });
            }
        }
    };
}
