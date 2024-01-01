pub fn Runner(comptime T: type) type {
    return struct {
        pub const EventType = T.EventType;
        pub const StateType = T.StateType;
        machine: *const T,
        state: StateType = 0,

        pub fn init(machine: *const T) @This() {
            return .{ .machine = machine };
        }

        pub fn step(self: *@This(), input: EventType) !void {
            if (try self.spec.destinationForEvent(self.state, input)) |to| {
                self.state = to;
            } else {
                return error.InvalidTransition;
            }
        }
    };
}
