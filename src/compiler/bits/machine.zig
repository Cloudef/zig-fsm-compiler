/// Mutable machine used by the compiler
/// All optimized immutable machines are derived from this
pub const Mutable = @import("machine/mutable.zig").Machine(usize, usize);
