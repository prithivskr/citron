const std = @import("std");

pub fn BoundedVec(comptime T: type, comptime cap: usize) type {
    return struct {
        const Self = @This();

        data: [cap]T = undefined,
        len: usize = 0,

        pub fn push(self: *Self, value: T) !void {
            if (self.len >= cap) return error.OutOfCapacity;
            self.data[self.len] = value;
            self.len += 1;
        }

        pub fn clear(self: *Self) void {
            self.len = 0;
        }

        pub fn items(self: *const Self) []const T {
            return self.data[0..self.len];
        }
    };
}

pub fn Bitset(comptime n_bits: usize) type {
    const word_count = (n_bits + 63) / 64;
    return struct {
        const Self = @This();

        words: [word_count]u64 = [_]u64{0} ** word_count,

        pub fn set(self: *Self, idx: usize) bool {
            const wi = idx / 64;
            const mask: u64 = (@as(u64, 1) << @intCast(idx % 64));
            const had = (self.words[wi] & mask) != 0;
            self.words[wi] |= mask;
            return !had;
        }

        pub fn has(self: Self, idx: usize) bool {
            const wi = idx / 64;
            const mask: u64 = (@as(u64, 1) << @intCast(idx % 64));
            return (self.words[wi] & mask) != 0;
        }

        pub fn isEmpty(self: Self) bool {
            for (self.words) |w| {
                if (w != 0) return false;
            }
            return true;
        }

        pub fn eql(a: Self, b: Self) bool {
            return std.mem.eql(u64, &a.words, &b.words);
        }

        pub fn unionWith(self: *Self, other: Self) bool {
            var changed = false;
            for (&self.words, 0..) |*w, i| {
                const prev = w.*;
                w.* |= other.words[i];
                if (w.* != prev) changed = true;
            }
            return changed;
        }
    };
}
