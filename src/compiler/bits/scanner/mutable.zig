const std = @import("std");
const machine = @import("../machine.zig");

machines: std.ArrayListUnmanaged(machine.Mutable) = .{},

pub fn init() @This() {
    return .{};
}

pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    for (self.machines.items) |*m| m.deinit(allocator);
    self.machines.deinit(allocator);
}

pub fn append(self: *@This(), allocator: std.mem.Allocator, fsm: machine.Mutable) !void {
    try self.machines.append(allocator, fsm);
}
