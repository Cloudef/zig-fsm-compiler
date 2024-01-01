const std = @import("std");

pub fn MultiDynamicBitSetUnmanaged(comptime Bitsets: anytype) type {
    return struct {
        bs: Bitsets = undefined,

        pub fn init(allocator: std.mem.Allocator, size: usize) !@This() {
            var self: @This() = .{};
            inline for (std.meta.fields(Bitsets)) |f| {
                @field(self.bs, f.name) = try std.DynamicBitSetUnmanaged.initEmpty(allocator, size);
            }
            return self;
        }

        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            inline for (std.meta.fields(Bitsets)) |f| {
                @field(self.bs, f.name).deinit(allocator);
            }
        }

        pub fn clone(self: @This(), allocator: std.mem.Allocator) !@This() {
            var new: @This() = .{};
            inline for (std.meta.fields(Bitsets)) |f| {
                @field(new.bs, f.name) = try @field(self.bs, f.name).clone(allocator);
            }
            return new;
        }

        pub fn inherit(self: *@This(), src: usize, dst: usize) void {
            inline for (std.meta.fields(Bitsets)) |f| {
                if (@field(self.bs, f.name).isSet(src)) @field(self.bs, f.name).set(dst);
            }
        }

        pub fn inheritFromOther(self: *@This(), other: @This(), src: usize, dst: usize) void {
            inline for (std.meta.fields(Bitsets)) |f| {
                if (@field(other.bs, f.name).isSet(src)) @field(self.bs, f.name).set(dst);
            }
        }

        pub fn eql(self: @This(), a: usize, b: usize) bool {
            inline for (std.meta.fields(Bitsets)) |f| {
                if (@field(self.bs, f.name).isSet(a) != @field(self.bs, f.name).isSet(b)) return false;
            }
            return true;
        }

        pub fn clear(self: *@This(), index: usize) void {
            inline for (std.meta.fields(Bitsets)) |f| {
                @field(self.bs, f.name).unset(index);
            }
        }
    };
}
