const std = @import("std");
const rl_scanner = @import("todo-replace/scanner.zig");
pub const machine = @import("bits/machine.zig");
pub const scanner = @import("bits/scanner.zig");
const common = @import("common");
const log = std.log.scoped(.compiler);

fn postfixToInfix(allocator: std.mem.Allocator, tokens: []const Token) ![]const Token {
    const Operator = struct {
        type: TokenType,
        precedence: u8,
        assoc: enum { left, right } = .left,
        unary: bool = false,
        grouping: union (enum) {
            none: void,
            start: void,
            end: []const TokenType,
            preserved: []const TokenType,
        } = .none,
    };

    const operators: []const Operator = &.{
        // Join
        .{ .type = .TK_Comma, .precedence = 1 }, // ,
        // Union, Intersection and Substraction
        .{ .type = .TK_Union, .precedence = 2 }, // |
        //.{ .type = .TK_And, .precedence = 2 }, // &
        //.{ .type = .TK_Dash, .precedence = 2 }, // -
        .{ .type = .TK_DashDash, .precedence = 2 }, // --
        // Concatenation
        .{ .type = .TK_Dot, .precedence = 3 }, // .
        .{ .type = .TK_LtColon , .precedence = 3 }, // <:
        .{ .type = .TK_ColonGt, .precedence = 3 }, // :>
        .{ .type = .TK_ColonGtGt, .precedence = 3 }, // :>>
        // Label
        // .{ .type = .TK_Colon, .precedence = 4 }, // :
        // Epsilon Transition
        .{ .type = .TK_Arrow, .precedence = 5 }, // ->
        // // Transition Actions and Priorities
        // .{ .type = .TK_Gt, .precedence = 6, .unary = true }, // <
        // .{ .type = .TK_At, .precedence = 6, .unary = true }, // @
        // .{ .type = .TK_Dollar, .precedence = 6, .unary = true }, // $
        // .{ .type = .TK_Percent, .precedence = 6, .unary = true }, // %
        // // EOF Actions
        // .{ .type = .TK_StartEOF, .precedence = 6, .unary = true }, // >/
        // .{ .type = .TK_AllEOF, .precedence = 6, .unary = true }, // $/
        // .{ .type = .TK_FinalEOF, .precedence = 6, .unary = true }, // %/
        // .{ .type = .TK_NotStartEOF, .precedence = 6, .unary = true }, // </
        // .{ .type = .TK_NotFinalEOF, .precedence = 6, .unary = true }, // @/
        // .{ .type = .TK_MiddleEOF, .precedence = 6, .unary = true }, // <>/
        // // Global Error Actions
        // .{ .type = .TK_StartGblError, .precedence = 6, .unary = true }, // >!
        // .{ .type = .TK_AllGblError, .precedence = 6, .unary = true }, // $!
        // .{ .type = .TK_FinalGblError, .precedence = 6, .unary = true }, // %!
        // .{ .type = .TK_NotStartGblError, .precedence = 6, .unary = true }, // <!
        // .{ .type = .TK_NotFinalGblError, .precedence = 6, .unary = true }, // @!
        // .{ .type = .TK_MiddleGblError, .precedence = 6, .unary = true }, // <>!
        // // Local Error Actions
        // .{ .type = .TK_StartLocalError, .precedence = 6, .unary = true }, // >^
        // .{ .type = .TK_AllLocalError, .precedence = 6, .unary = true }, // $^
        // .{ .type = .TK_FinalLocalError, .precedence = 6, .unary = true }, // %^
        // .{ .type = .TK_NotStartLocalError, .precedence = 6, .unary = true }, // <^
        // .{ .type = .TK_NotFinalLocalError, .precedence = 6, .unary = true }, // @^
        // .{ .type = .TK_MiddleLocalError, .precedence = 6, .unary = true }, // <>^
        // // To-State Actions
        // .{ .type = .TK_StartToState, .precedence = 6, .unary = true }, // >~
        // .{ .type = .TK_AllToState, .precedence = 6, .unary = true }, // $~
        // .{ .type = .TK_FinalToState, .precedence = 6, .unary = true }, // %~
        // .{ .type = .TK_NotStartToState, .precedence = 6, .unary = true }, // <~
        // .{ .type = .TK_NotFinalToState, .precedence = 6, .unary = true }, // @~
        // .{ .type = .TK_MiddleToState, .precedence = 6, .unary = true }, // <>~
        // // From-State Actions
        // .{ .type = .TK_StartFromState, .precedence = 6, .unary = true }, // >*
        // .{ .type = .TK_AllFromState, .precedence = 6, .unary = true }, // $*
        // .{ .type = .TK_FinalFromState, .precedence = 6, .unary = true }, // %*
        // .{ .type = .TK_NotStartFromState, .precedence = 6, .unary = true }, // <*
        // .{ .type = .TK_NotFinalFromState, .precedence = 6, .unary = true }, // @*
        // .{ .type = .TK_MiddleFromState, .precedence = 6, .unary = true }, // <>*
        // Repetition
        .{ .type = .RE_Star, .precedence = 7, .assoc = .right, .unary = true }, // *
        .{ .type = .TK_StarStar, .precedence = 7, .assoc = .right, .unary = true }, // **
        .{ .type = .RE_Maybe, .precedence = 7, .assoc = .right, .unary = true }, // ?
        .{ .type = .RE_Repeat, .precedence = 7, .assoc = .right, .unary = true }, // +
        // Negation and Character-Level Negation
        // TODO: unary
        .{ .type = .TK_Negation, .precedence = 8, .unary = true }, // !
        // Grouping
        .{ .type = .TK_ParenOpen, .precedence = 9, .grouping = .{ .preserved = &.{ .TK_ParenClose } } }, // (
        .{ .type = .RE_SqOpen, .precedence = 9, .grouping = .{ .preserved = &.{ .RE_SqClose } } }, // [
        .{ .type = .RE_SqOpenNeg, .precedence = 9, .grouping = .{ .preserved = &.{ .RE_SqClose } } }, // [^
        .{ .type = .RE_Slash, .precedence = 9, .grouping = .{ .preserved = &.{ .RE_Slash } } }, // /
    };

    // TODO: optimize, instead of storing Operator just store the type
    var opstack = std.BoundedArray(Operator, 32){};
    var outputs = std.ArrayListUnmanaged(Token){};
    errdefer outputs.deinit(allocator);

    var iter = TokenIterator{.tokens = tokens};
    while (iter.next()) |tok| {
        const maybe_op: ?Operator = blk: {
            for (operators) |op| if (tok.type == op.type) break :blk op;
            break :blk null;
        };
        if (maybe_op) |op| {
            switch (op.grouping) {
                .preserved => |closers| {
                    const mark = iter.marker();
                    _ = try iter.untilType(closers);
                    try outputs.appendSlice(allocator, iter.capture(mark - 1, 0));
                },
                .start => try opstack.append(op),
                .end => |openers| {
                    var top: Operator = op;
                    while (opstack.len > 0) {
                        top = opstack.constSlice()[opstack.len - 1];
                        if (std.mem.count(TokenType, openers, &.{top.type}) > 0) break;
                        std.debug.assert(top.grouping == .none);
                        try outputs.append(allocator, .{ .type = opstack.pop().type });
                    }
                    if (std.mem.count(TokenType, openers, &.{top.type}) == 0) return error.UnmatchedGroup;
                    _ = opstack.pop();
                },
                .none => {
                    while (opstack.len > 0) {
                        const last = opstack.constSlice()[opstack.len - 1];
                        if (last.grouping == .none and
                            (last.precedence > op.precedence or
                             (last.precedence == op.precedence and op.assoc == .left))) {
                            try outputs.append(allocator, .{ .type = opstack.pop().type });
                        } else break;
                    }
                    if (op.unary and op.assoc == .right) {
                        try outputs.append(allocator, tok);
                    } else {
                        try opstack.append(op);
                    }
                },
            }
        } else {
            try outputs.append(allocator, tok);
            while (opstack.len > 0) {
                const last = opstack.constSlice()[opstack.len - 1];
                if (last.unary and last.assoc == .left) {
                    try outputs.append(allocator, .{ .type = opstack.pop().type });
                } else break;
            }
        }
    }

    while (opstack.popOrNull()) |op| {
        if (op.grouping != .none) return error.UmatchedGroup;
        try outputs.append(allocator, .{ .type = op.type });
    }

    // log.warn("infix: {}", .{
    //     std.json.fmt(outputs.items, .{.whitespace = .indent_2})
    // });

    return outputs.toOwnedSlice(allocator);
}

const Token = rl_scanner.Token;
const TokenType = rl_scanner.TokenType;
const TokenIterator = struct {
    tokens: []const Token,
    next_index: usize = 0,

    pub fn reset(self: *@This()) void {
        self.next_index = 0;
    }

    pub fn marker(self: @This()) usize {
        return self.next_index;
    }

    pub fn capture(self: @This(), start: usize, end_offset: usize) []const Token {
        const end: usize = self.next_index - end_offset;
        return self.tokens[start..end];
    }

    pub fn prev(self: @This()) !Token {
        if (self.next_index <= 1) return error.NoPreviousToken;
        return self.tokens[self.next_index - 2];
    }

    pub fn peek(self: *@This(), offset: usize) ?Token {
        if (self.tokens.len > self.next_index + offset) {
            const token = self.tokens[self.next_index + offset];
            return token;
        } else {
            return null;
        }
    }

    pub fn isSequence(self: *@This(), seq: []const TokenType) bool {
        for (seq, 0..) |ex, i| {
            if (self.peek(i)) |tok| {
                if (tok.type != ex) return false;
            } else {
                return false;
            }
        }
        return true;
    }

    pub fn next(self: *@This()) ?Token {
        const token = self.peek(0);
        if (token != null) self.next_index += 1;
        return token;
    }

    pub fn nextNotNull(self: *@This()) !Token {
        const token = self.next();
        if (token == null) return error.Unexpected;
        return token.?;
    }

    pub fn untilType(self: *@This(), expected: []const TokenType) !Token {
        while (self.next()) |tok| for (expected) |ex| if (tok.type == ex) return tok;
        return error.Unexpected;
    }

    pub fn untilClosed(self: *@This(), comptime opening: []const TokenType, comptime closing: TokenType) !Token {
        if (self.next_index < 1) return error.Unexpected;
        var tok = self.tokens[self.next_index - 1];
        if (std.mem.count(TokenType, opening, &.{tok.type}) == 0) return error.Unexpected;
        var depth: usize = 1;
        while (depth > 0) {
            tok = try self.untilType(opening ++ .{closing});
            if (std.mem.count(TokenType, opening, &.{tok.type}) > 0) depth += 1
            else if (tok.type == closing) depth -= 1;
        }
        return tok;
    }

    pub fn nextOfType(self: *@This(), expected: []const TokenType) !Token {
        if (self.next()) |tok| {
            for (expected) |ex| if (tok.type == ex) return tok;
            log.err("unexpected token: got {}, expected {}", .{tok.type, std.json.fmt(expected, .{})});
        } else {
            log.err("unexpected token: got null, expected {}", .{std.json.fmt(expected, .{})});
        }
        return error.Unexpected;
    }
};

const Compiler = struct {
    const Action = struct {
        body: []const Token,
    };

    const LazyMachine = union (enum) {
        expr: []const Token,
        compiled: machine.Mutable,
    };

    const Decl = struct {
        namespace: []const u8,
        name: []const u8,
        value: union (enum) {
            none: void,
            action: Action,
            defined: LazyMachine,
            instanced: union (enum) {
                machine: machine.Mutable,
                scanner: scanner.Mutable,
            },
        },

        fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            switch (self.value) {
                .none => {},
                .action => {},
                .defined => |*lazy| switch (lazy.*) {
                    .expr => {},
                    .compiled => |*m| m.deinit(allocator),
                },
                .instanced => |*i| switch (i.*) {
                    .machine => |*m| m.deinit(allocator),
                    .scanner => |*s| s.deinit(allocator),
                },
            }
        }
    };

    const Builtin = struct {
        name: []const u8,
        machine: machine.Mutable,
    };

    decls: std.ArrayListUnmanaged(Decl) = .{},
    builtins: std.ArrayListUnmanaged(Builtin) = .{},

    fn RangeSlice(comptime T: type, comptime min: T, comptime max: T) [@as(usize, max - min) + 1]T {
        comptime std.debug.assert(min <= max);
        const sz: usize = @as(usize, max - min) + 1;
        comptime var slice: [sz]T = undefined;
        inline for (slice[0..], 0..) |*s, i| s.* = min + i;
        return slice;
    }

    fn init(allocator: std.mem.Allocator) !@This() {
        var self: @This() = .{};
        const ascii = comptime RangeSlice(u8, 0, 127);
        const extend = comptime RangeSlice(u8, 0, 255);
        const a_z = comptime RangeSlice(u8, 'a', 'z');
        const A_Z = comptime RangeSlice(u8, 'A', 'Z');
        const digit = comptime RangeSlice(u8, '0', '9');
        const xdigit = comptime digit ++ RangeSlice(u8, 'A', 'F') ++ RangeSlice(u8, 'a', 'f');
        const cntrl = comptime RangeSlice(u8, 0, 31);
        const graph = comptime RangeSlice(u8, '!', '~');
        const print = comptime RangeSlice(u8, ' ', '~');
        const punct = comptime RangeSlice(u8, '!', '/') ++ RangeSlice(u8, ':', '@') ++ RangeSlice(u8, '[', '`') ++ RangeSlice(u8, '{', '~');
        try self.builtins.append(allocator, .{
            .name = "ascii",
            // Ascii characters. 0..127
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, ascii[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "extend",
            // Ascii extended characters. 0..255
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, extend[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "lower",
            // Lowercase characters. [a-z]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, a_z[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "upper",
            // Uppercase characters. [A-Z]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, A_Z[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "alpha",
            // Alphabetic characters. [A-Za-z]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, (A_Z ++ a_z)[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "digit",
            // Digits. [0-9]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, digit[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "alnum",
            // Alpha numerics. [0-9A-Za-z]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, (digit ++ A_Z ++ a_z)[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "xdigit",
            // Hexadecimal digits. [0-9A-Fa-f]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, xdigit[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "cntrl",
            // Control characters. 0..31
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, cntrl[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "graph",
            // Graphical characters. [!-~]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, graph[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "print",
            // Printable characters. [ -~]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, print[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "punct",
            // Punctuation. Graphical characters that are not alphanumerics. [!-/:-@[-‘{-~]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, punct[0..], false),
        });
        try self.builtins.append(allocator, .{
            .name = "space",
            // hitespace. [\t\v\f\n\r ]
            .machine = try machine.Mutable.fromUnionSlice(u8, allocator, "\t\n\x0B\x0C\r ", false),
        });
        try self.builtins.append(allocator, .{
            .name = "zlen",
            // Zero length string. ""
            .machine = try machine.Mutable.zeroLength(allocator),
        });
        // TODO: all below is not correct
        try self.builtins.append(allocator, .{
            .name = "any",
            // Matches any input.
            .machine = try machine.Mutable.zeroLength(allocator),
        });
        try self.builtins.append(allocator, .{
            .name = "empty",
            // Empty set. Matches nothing. ^any
            .machine = try machine.Mutable.zeroLength(allocator),
        });
        return self;
    }

    fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        for (self.decls.items) |*d| d.deinit(allocator);
        self.decls.deinit(allocator);
        for (self.builtins.items) |*b| b.machine.deinit(allocator);
        self.builtins.deinit(allocator);
        self.* = undefined;
    }

    fn declare(self: *@This(), allocator: std.mem.Allocator, decl: Decl) !void {
        try self.decls.append(allocator, decl);
    }

    fn findDecl(self: *@This(), namespace: []const u8, name: []const u8) ?*Decl {
        for (self.decls.items) |*d| {
            if ((namespace.len == 0 or std.mem.eql(u8, namespace, d.namespace)) and std.mem.eql(u8, name, d.name)) {
                return d;
            }
        }
        return null;
    }

    fn findDeclAction(self: *@This(), namespace: []const u8, name: []const u8) anyerror!?Action {
        if (self.findDecl(namespace, name)) |decl| {
            switch (decl.value) {
                .action => |a| return a,
                else => return null,
            }
        }
        return null;
    }

    fn findDeclDefined(self: *@This(), allocator: std.mem.Allocator, namespace: []const u8, name: []const u8) anyerror!?machine.Mutable {
        for (self.builtins.items) |b| if (std.mem.eql(u8, b.name, name)) return b.machine;
        if (self.findDecl(namespace, name)) |decl| {
            switch (decl.value) {
                .defined => |*d| switch (d.*) {
                    .expr => |postfix| {
                        const expr = try postfixToInfix(allocator, postfix);
                        defer allocator.free(expr);
                        var iter: TokenIterator = .{.tokens = expr};
                        d.* = .{ .compiled = try self.compileExpr(allocator, namespace, &iter, false) };
                        return d.compiled;
                    },
                    .compiled => |m| return m,
                },
                else => return null,
            }
        }
        return null;
    }

    fn emitError(_: *@This(), token: Token) !void {
        log.err("unexpected token: {}", .{token});
        return error.Unexpected;
    }

    fn compileUnion(self: *@This(), allocator: std.mem.Allocator, tokens: *TokenIterator, neg: bool, ignore_case: bool) !machine.Mutable {
        var inputs = common.ArraySetUnmanaged(u8){};
        defer inputs.deinit(allocator);
        while (tokens.next()) |tok| switch (tok.type) {
            .RE_Char => {
                if (tokens.isSequence(&.{.RE_Dash, .RE_Char})) {
                    const start = tok.payload.chr;
                    _ = try tokens.nextOfType(&.{.RE_Dash});
                    const end = (try tokens.nextOfType(&.{.RE_Char})).payload.chr;
                    if (start < end) {
                        for (start..end + 1) |i| try inputs.set(allocator, @truncate(i));
                    } else {
                        for (end..start + 1) |i| try inputs.set(allocator, @truncate(i));
                    }
                } else {
                    try inputs.set(allocator, tok.payload.chr);
                }
            },
            .RE_Dash => try inputs.set(allocator, '-'),
            .RE_Repeat => try inputs.set(allocator, '+'),
            .RE_Star => try inputs.set(allocator, '*'),
            else => try self.emitError(tok),
        };
        std.sort.heap(u8, inputs.container.items, {}, std.sort.asc(u8));
        var fsm = try machine.Mutable.fromUnionSlice(u8, allocator, inputs.constSlice(), ignore_case);
        if (neg) fsm.applyNegation();
        return fsm;
    }

    fn compileReExpr(self: *@This(), allocator: std.mem.Allocator, tokens: *TokenIterator, ignore_case: bool) !machine.Mutable {
        var stack = std.ArrayListUnmanaged(machine.Mutable){};

        defer {
            for (stack.items) |*m| m.deinit(allocator);
            stack.deinit(allocator);
        }

        while (tokens.next()) |tok| {
            const maybe_last: ?*machine.Mutable = if (stack.items.len > 0) &stack.items[stack.items.len - 1] else null;
            switch (tok.type) {
                .RE_Char => try stack.append(allocator, try machine.Mutable.fromChar(u8, allocator, tok.payload.chr, ignore_case)),
                .RE_Dot => try stack.append(allocator, try (try self.findDeclDefined(allocator, "", "any")).?.clone(allocator)),
                .RE_SqOpen, .RE_SqOpenNeg => {
                    const marker = tokens.marker();
                    _ = try tokens.untilClosed(&.{.RE_SqOpen, .RE_SqOpenNeg}, .RE_SqClose);
                    var iter: TokenIterator = .{ .tokens = tokens.capture(marker, 1) };
                    try stack.append(allocator, try self.compileUnion(allocator, &iter, tok.type == .RE_SqOpenNeg, ignore_case));
                },
                else => if (maybe_last) |last| switch (tok.type) {
                    .RE_Star => try last.applyKleeneStar(allocator),
                    .RE_Repeat => {
                        var fsm = try last.clone(allocator);
                        try fsm.applyKleeneStar(allocator);
                        try stack.append(allocator, fsm);
                    },
                    else => try self.emitError(tok),
                } else try self.emitError(tok),
            }
        }

        return machine.Mutable.combine(allocator, stack.items, .{.mode = .concatetate});
    }

    fn compileExpr(self: *@This(), allocator: std.mem.Allocator, namespace: []const u8, tokens: *TokenIterator, in_paren: bool) !machine.Mutable {
        var stack = std.ArrayListUnmanaged(machine.Mutable){};

        defer {
            for (stack.items) |*m| m.deinit(allocator);
            stack.deinit(allocator);
        }

        while (tokens.next()) |tok| {
            const maybe_last: ?*machine.Mutable = if (stack.items.len > 0) &stack.items[stack.items.len - 1] else null;
            switch (tok.type) {
                .TK_Gt, .TK_At, .TK_Dollar, .TK_Percent,
                .TK_FinalEOF, .TK_FinalFromState, .TK_FinalGblError, .TK_FinalLocalError,
                .TK_FinalToState => {
                    const ctok = try tokens.nextNotNull();
                    switch (ctok.type) {
                        // action
                        .TK_Word => {
                            log.info("action {} {s}", .{tok.type, ctok.payload.str});
                        },
                        // static prio
                        .TK_UInt => {
                            log.info("prio {} {s}", .{tok.type, ctok.payload.str});
                        },
                        // defined prio
                        .TK_ParenTransitionOpen => {
                            const name = try tokens.nextOfType(&.{.TK_Word});
                            _ = try tokens.nextOfType(&.{.TK_Comma});
                            const prio = try tokens.nextOfType(&.{.TK_UInt});
                            _ = try tokens.nextOfType(&.{.TK_ParenTransitionClose});
                            log.info("prio {} {s} {s}", .{tok.type, name.payload.str, prio.payload.str});
                        },
                        else => try self.emitError(ctok),
                    }
                },
                .TK_ParenOpen => {
                    if (in_paren) {
                        _ = try tokens.untilClosed(&.{.TK_ParenOpen}, .TK_ParenClose);
                    } else {
                        var ctok = tok;
                        var markers: std.BoundedArray(usize, 32) = .{};
                        try markers.append(tokens.marker());
                        while (markers.len > 0) {
                            ctok = try tokens.untilType(&.{.TK_ParenOpen, .TK_ParenClose});
                            switch (ctok.type) {
                                .TK_ParenOpen => try markers.append(tokens.marker()),
                                .TK_ParenClose => {
                                    const expr = try postfixToInfix(allocator, tokens.capture(markers.pop(), 1));
                                    defer allocator.free(expr);
                                    var iter: TokenIterator = .{ .tokens = expr };
                                    try stack.append(allocator, try self.compileExpr(allocator, namespace, &iter, true));
                                },
                                else => unreachable,
                            }
                        }
                    }
                },
                .TK_Word => {
                    const maybe_fsm = try self.findDeclDefined(allocator, namespace, tok.payload.str);
                    if (maybe_fsm) |fsm| {
                        try stack.append(allocator, try fsm.clone(allocator));
                    } else {
                        try self.emitError(tok);
                    }
                },
                .TK_UInt => {
                    const num = try std.fmt.parseInt(machine.Mutable.EventScalar, tok.payload.str, 10);
                    try stack.append(allocator, try machine.Mutable.fromScalar(machine.Mutable.EventScalar, allocator, num));
                },
                .TK_Literal => {
                    const fsm = blk: {
                        const lit = tok.payload.str;
                        const ignore_case = lit.len > 0 and lit[lit.len - 1] == 'i';
                        var copy = try allocator.dupe(u8, lit[1..lit.len - 1 - @intFromBool(ignore_case)]);
                        defer allocator.free(copy);
                        var reps: usize = 0;
                        reps += std.mem.replace(u8, copy, "\\a", "\x07", copy);
                        reps += std.mem.replace(u8, copy, "\\b", "\x08", copy);
                        reps += std.mem.replace(u8, copy, "\\t", "\t", copy);
                        reps += std.mem.replace(u8, copy, "\\n", "\n", copy);
                        reps += std.mem.replace(u8, copy, "\\v", "\x0B", copy);
                        reps += std.mem.replace(u8, copy, "\\f", "\x0C", copy);
                        reps += std.mem.replace(u8, copy, "\\r", "\r", copy);
                        break :blk try machine.Mutable.fromSlice(u8, allocator, copy[0..copy.len - reps], ignore_case);
                    };
                    try stack.append(allocator, fsm);
                },
                .RE_Slash => {
                    const marker = tokens.marker();
                    const end = try tokens.untilType(&.{.RE_Slash});
                    var iter: TokenIterator = .{ .tokens = tokens.capture(marker, 1) };
                    const ignore_case = end.payload.str[end.payload.str.len - 1] == 'i';
                    try stack.append(allocator, try self.compileReExpr(allocator, &iter, ignore_case));
                },
                .RE_SqOpen, .RE_SqOpenNeg => {
                    const marker = tokens.marker();
                    _ = try tokens.untilClosed(&.{.RE_SqOpen, .RE_SqOpenNeg}, .RE_SqClose);
                    var iter: TokenIterator = .{ .tokens = tokens.capture(marker, 1) };
                    try stack.append(allocator, try self.compileUnion(allocator, &iter, tok.type == .RE_SqOpenNeg, false));
                },
                .TK_Union => {
                    var a = stack.pop();
                    defer a.deinit(allocator);
                    var b = stack.pop();
                    defer b.deinit(allocator);
                    try stack.append(allocator, try machine.Mutable.combine(allocator, &.{ a, b }, .{.mode = .@"union"}));
                },
                // TODO: handle intersection
                // TODO: handle difference
                // TODO: handle strong difference
                // TODO: handle character level negation
                else => if (maybe_last) |last| switch (tok.type) {
                    .RE_Star => try last.applyKleeneStar(allocator),
                    .RE_Repeat => {
                        var fsm = try last.clone(allocator);
                        try fsm.applyKleeneStar(allocator);
                        try stack.append(allocator, fsm);
                    },
                    .RE_Maybe => {
                        var zero = try machine.Mutable.zeroLength(allocator);
                        defer zero.deinit(allocator);
                        var popd = stack.pop();
                        defer popd.deinit(allocator);
                        try stack.append(allocator, try machine.Mutable.combine(allocator, &.{ popd, zero }, .{.mode = .@"union"}));
                    },
                    .TK_Negation => last.applyNegation(),
                    else => try self.emitError(tok),
                } else try self.emitError(tok),
            }
        }

        return machine.Mutable.combine(allocator, stack.items, .{.mode = .concatetate});
    }

    fn compileScanner(self: *@This(), allocator: std.mem.Allocator, namespace: []const u8, tokens: *TokenIterator) !scanner.Mutable {
        var scan = scanner.Mutable.init();
        errdefer scan.deinit(allocator);
        while (tokens.peek(0) != null) {
            const marker = tokens.marker();
            const delim = try tokens.untilType(&.{.TK_DoubleArrow, .TK_SemiColon});
            const expr = try postfixToInfix(allocator, tokens.capture(marker, 1));
            defer allocator.free(expr);
            var iter: TokenIterator = .{ .tokens = expr };
            if (delim.type == .TK_DoubleArrow) {
                _ = try tokens.untilType(&.{.TK_CurlyOpen});
                _ = try tokens.untilType(&.{.TK_CurlyClose});
            }
            // TODO: scanner action
            try scan.append(allocator, try self.compileExpr(allocator, namespace, &iter, false));
            if (delim.type == .TK_DoubleArrow) {
                _ = try tokens.untilType(&.{.TK_SemiColon});
            }
        }
        return scan;
    }

    fn compile(allocator: std.mem.Allocator, tokens: *TokenIterator, _: Options) ![]Instanced {
        var compiler = try @This().init(allocator);
        defer compiler.deinit(allocator);
        log.warn("{s}", .{std.json.fmt(tokens.tokens, .{.whitespace = .indent_1})});

        var mname: ?[]const u8 = null;
        while (tokens.next()) |tok| switch (tok.type) {
            .KW_Machine => {
                const name = try tokens.nextOfType(&.{.TK_Word});
                _ = try tokens.nextOfType(&.{.TK_SemiColon});
                mname = name.payload.str;
                log.info("machine = {s}", .{mname.?});
            },
            .KW_Action => {
                const name = try tokens.nextOfType(&.{.TK_Word});
                _ = try tokens.nextOfType(&.{.TK_CurlyOpen});
                const marker = tokens.marker();
                _ = try tokens.untilType(&.{.TK_CurlyClose});
                log.info("action {s}", .{name.payload.str});
                try compiler.declare(allocator, .{
                    .namespace = mname.?,
                    .name = name.payload.str,
                    .value = .{ .action = .{
                        .body = tokens.capture(marker, 1)
                    }},
                });
            },
            .TK_Word => {
                const kind = try tokens.nextOfType(&.{.TK_Equals, .TK_ColonEquals, .TK_BarEquals});
                const is_scanner = blk: {
                    if (kind.type != .TK_ColonEquals) break :blk false;
                    break :blk if (tokens.peek(0)) |next| next.type == .TK_BarStar else false;
                };
                if (is_scanner) _ = try tokens.nextOfType(&.{.TK_BarStar});
                const marker = tokens.marker();
                _ = try tokens.untilType(&.{.TK_SemiColon});
                if (is_scanner) _ = try tokens.untilType(&.{.TK_StarBar});
                const body = tokens.capture(marker, 1);
                log.info("expr {s}:", .{tok.payload.str});
                log.info("{}", .{std.json.fmt(body, .{.whitespace = .indent_2})});
                switch (kind.type) {
                    .TK_Equals => try compiler.declare(allocator, .{
                        .namespace = mname.?,
                        .name = tok.payload.str,
                        .value = .{ .defined = .{ .expr = body } },
                    }),
                    .TK_ColonEquals => {
                        if (is_scanner) {
                            var iter: TokenIterator = .{ .tokens = body };
                            var scan = try compiler.compileScanner(allocator, mname.?, &iter);
                            errdefer scan.deinit(allocator);
                            try compiler.declare(allocator, .{
                                .namespace = mname.?,
                                .name = tok.payload.str,
                                .value = .{ .instanced = .{ .scanner = scan } },
                            });
                            _ = try tokens.nextOfType(&.{.TK_SemiColon});
                        } else {
                            const expr = try postfixToInfix(allocator, body);
                            defer allocator.free(expr);
                            var iter: TokenIterator = .{ .tokens = expr };
                            var fsm = try compiler.compileExpr(allocator, mname.?, &iter, false);
                            errdefer fsm.deinit(allocator);
                            try compiler.declare(allocator, .{
                                .namespace = mname.?,
                                .name = tok.payload.str,
                                .value = .{ .instanced = .{ .machine = fsm } },
                            });
                        }
                    },
                    .TK_BarEquals => return error.NotYetSupported,
                    else => unreachable,
                }
            },
            else => try compiler.emitError(tok),
        };

        var instanced = std.ArrayListUnmanaged(Instanced){};
        errdefer {
            for (instanced.items) |*i| {
                allocator.free(i.namespace);
                allocator.free(i.name);
                switch (i.value) {
                    .machine => |*m| m.deinit(allocator),
                    .scanner => |*s| s.deinit(allocator),
                }
            }
            instanced.deinit(allocator);
        }

        for (compiler.decls.items) |*decl| switch (decl.value) {
            .instanced => |i| {
                try instanced.append(allocator, .{
                    .namespace = try allocator.dupe(u8, decl.namespace),
                    .name = try allocator.dupe(u8, decl.name),
                    .value = switch (i) {
                        .machine => |m| .{ .machine = m },
                        .scanner => |s| .{ .scanner = s },
                    },
                });
                decl.value = .none;
            },
            else => {},
        };

        return instanced.toOwnedSlice(allocator);
    }
};

pub const Options = struct {};

pub const Instanced = struct {
    namespace: []const u8,
    name: []const u8,
    value: union (enum) {
        machine: machine.Mutable,
        scanner: scanner.Mutable,
    },
};

pub const Result = struct {
    instanced: []Instanced,

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        for (self.instanced) |*i| {
            allocator.free(i.namespace);
            allocator.free(i.name);
            switch (i.value) {
                .machine => |*m| m.deinit(allocator),
                .scanner => |*s| s.deinit(allocator),
            }
        }
        allocator.free(self.instanced);
    }
};

pub fn compile(allocator: std.mem.Allocator, slice: []const u8, opts: Options) !Result {
    const result = try rl_scanner.parse(allocator, slice);
    defer result.deinit(allocator);
    var iter = TokenIterator{.tokens = result.tokens};
    return .{ .instanced = try Compiler.compile(allocator, &iter, opts) };
}

pub fn compileFromReader(allocator: std.mem.Allocator, reader: std.io.AnyReader, opts: Options) !Result {
    const slice = try reader.readAllAlloc(allocator, 1e+9);
    defer allocator.free(slice);
    return try compile(allocator, slice, opts);
}
