const std = @import("std");
const log = std.log.scoped(.scanner);

const c = @cImport({
    @cInclude("rlscan.h");
});

pub const TokenType = enum {
    unknown,
    IL_Comment,
    IL_Literal,
    IL_Symbol,
    IL_WhiteSpace,
    KW_Access,
    KW_Action,
    KW_AlphType,
    KW_Break,
    KW_Call,
    KW_Char,
    KW_CurState,
    KW_Entry,
    KW_Eof,
    KW_Err,
    KW_Exec,
    KW_Export,
    KW_From,
    KW_GetKey,
    KW_Goto,
    KW_Hold,
	KW_Write,
    KW_Import,
    KW_Include,
    KW_InWhen,
    KW_Length,
    KW_Lerr,
    KW_Machine,
    KW_Nbreak,
    KW_Ncall,
    KW_Next,
    KW_NfaPostPop,
    KW_NfaPrePush,
    KW_Nret,
    KW_OutWhen,
    KW_PChar,
    KW_PostPop,
    KW_PrePush,
    KW_Range,
    KW_Ret,
    KW_TargState,
    KW_To,
    KW_Variable,
    KW_When,
    RE_Char,
    RE_Dash,
    RE_Dot,
    RE_Slash,
    RE_SqClose,
    RE_SqOpen,
    RE_SqOpenNeg,
    RE_Star,
    RE_Repeat,
    RE_Maybe,
    TK_AllCond,
    TK_AllEOF,
    TK_AllFromState,
    TK_AllGblError,
    TK_AllLocalError,
    TK_AllToState,
    TK_Arrow,
    TK_Equals,
    TK_BarEquals,
    TK_BarStar,
    TK_StarBar,
    TK_CloseColon,
    TK_ColonCondOpen,
    TK_ColonCondPlusOpen,
    TK_ColonCondStarOpen,
    TK_ColonEquals,
    TK_ColonGt,
    TK_ColonGtGt,
    TK_ColonNfaOpen,
    TK_ColonNoMaxOpen,
    TK_SemiColon,
    TK_DashDash,
    TK_DotDot,
    TK_DotDotIndep,
    TK_DoubleArrow,
    TK_FinalEOF,
    TK_FinalFromState,
    TK_FinalGblError,
    TK_FinalLocalError,
    TK_FinalToState,
    TK_Hex,
    TK_LeavingCond,
    TK_Literal,
    TK_LtColon,
    TK_Middle,
    TK_MiddleEOF,
    TK_MiddleFromState,
    TK_MiddleGblError,
    TK_MiddleLocalError,
    TK_MiddleToState,
    TK_NameSep,
    TK_NotFinalEOF,
    TK_NotFinalFromState,
    TK_NotFinalGblError,
    TK_NotFinalLocalError,
    TK_NotFinalToState,
    TK_NotStartEOF,
    TK_NotStartFromState,
    TK_NotStartGblError,
    TK_NotStartLocalError,
    TK_NotStartToState,
    TK_StarStar,
    TK_StartCond,
    TK_StartEOF,
    TK_StartFromState,
    TK_StartGblError,
    TK_StartLocalError,
    TK_StartToState,
    TK_SubstRef,
    TK_UInt,
    TK_Word,
    TK_CurlyOpen,
    TK_CurlyClose,
    TK_ParenOpen,
    TK_ParenClose,
    TK_Comma,
    TK_At,
    TK_Percent,
    TK_Dollar,
    TK_Gt,
    TK_Union,
    TK_Negation,
    TK_Dot,

    // extra
    TK_ParenTransitionOpen,
    TK_ParenTransitionClose,
};

pub const Token = struct {
    type: TokenType,
    payload: union (enum) {
        none: void,
        chr: u8,
        str: []const u8,
    } = .none,

    pub fn isTransition(self: @This()) bool {
        return self.type == .TK_Gt or
               self.type == .TK_At or
               self.type == .TK_Dollar or
               self.type == .TK_Percent;
    }

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self.payload) {
            .none => writer.print("{}", .{ self.type }),
            .str => |str| writer.print("{}, {s}", .{ self.type, str }),
            .chr => |chr| writer.print("{}, {c}", .{ self.type, chr }),
        };
    }

    fn ClusterFormatter() type {
        return struct {
            tokens: []const Token,
            pub fn format(
                self: @This(),
                comptime _: []const u8,
                _: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                for (self.tokens) |tok| try writer.writeAll(tok.word.?);
            }
        };
    }

    pub fn fmtCluster(tokens: []const Token) ClusterFormatter() {
        return ClusterFormatter(){ .tokens = tokens };
    }
};

fn emitTokenZig(state: *State, ctype: c.TokenType, lit: [*c]const u8, is_char: bool) !void {
    const token: TokenType = @enumFromInt(ctype);
    if (lit != null) {
        if (is_char) {
            try state.tokens.append(state.allocator, .{ .type = token, .payload = .{ .chr = @truncate(@intFromPtr(lit)) } });
        } else {
            try state.tokens.append(state.allocator, .{ .type = token, .payload = .{ .str = std.mem.span(lit) } });
        }
    } else {
        if (state.is_transition_paren and token == .TK_ParenClose) {
            try state.tokens.append(state.allocator, .{ .type = .TK_ParenTransitionClose });
            state.is_transition_paren = false;
        } else if (token == .TK_ParenOpen and state.tokens.items.len > 0 and state.tokens.items[state.tokens.items.len-1].isTransition()) {
            try state.tokens.append(state.allocator, .{ .type = .TK_ParenTransitionOpen });
            state.is_transition_paren = true;
        } else {
            try state.tokens.append(state.allocator, .{ .type = token });
        }
    }
}

fn emitToken(scanner: [*c]c.Scanner, ctype: c.TokenType, lit: [*c]const u8, is_char: bool) callconv(.C) void {
    var state: *State = @alignCast(@ptrCast(scanner.*.ptr.?));
    if (state.failed != null) return;
    emitTokenZig(state, ctype, lit, is_char) catch |err| { state.failed = err; };
}

fn emitError(scanner: [*c]c.Scanner, msg: [*c]const u8) callconv(.C) void {
    var state: *State = @alignCast(@ptrCast(scanner.*.ptr.?));
    if (state.failed != null) return;
    state.failed = error.SyntaxError;
    log.err("{s}", .{msg});
}

const State = struct {
    allocator: std.mem.Allocator,
    tokens: std.ArrayListUnmanaged(Token) = .{},
    is_transition_paren: bool = false,
    failed: ?anyerror = null,
};

const Result = struct {
    tokens: []const Token,
    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.tokens);
    }
};

pub fn parse(allocator: std.mem.Allocator, src: []const u8) !Result {
    var state = State{ .allocator = allocator };
    var scanner: c.Scanner = .{
        .input = src.ptr,
        .input_len = @truncate(@as(isize, @bitCast(src.len))),
        .token = emitToken,
        .@"error" = emitError,
        .ptr = &state,
    };
    errdefer state.tokens.deinit(allocator);
    c.scan(&scanner);
    if (state.failed) |err| return err;
    return .{ .tokens = try state.tokens.toOwnedSlice(allocator) };
}
