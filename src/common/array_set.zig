const std = @import("std");

pub fn ArraySetUnmanaged(comptime T: type) type {
    return struct {
        container: std.ArrayListUnmanaged(T) = .{},

        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            self.container.deinit(allocator);
        }

        pub fn set(self: *@This(), allocator: std.mem.Allocator, value: T) !void {
            if (std.mem.count(T, self.container.items, &.{value}) > 0) return;
            try self.container.append(allocator, value);
        }

        pub fn setSlice(self: *@This(), allocator: std.mem.Allocator, slice: []const T) !void {
            for (slice) |v| try self.set(allocator, v);
        }

        pub fn toOwnedSlice(self: *@This(), allocator: std.mem.Allocator) ![]const T {
            return self.container.toOwnedSlice(allocator);
        }

        pub fn constSlice(self: @This()) []const T {
            return self.container.items;
        }

        pub fn clearRetainingCapacity(self: *@This()) void {
            self.container.clearRetainingCapacity();
        }
    };
}
