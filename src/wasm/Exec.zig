const std = @import("std");

pub const ValType = enum {
    i32,
    i64,
    f32,
    f64,
    pub fn format(self: ValType, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const val: []const u8 = switch (self) {
            .i32 => "i32",
            .i64 => "i64",
            .f32 => "f32",
            .f64 => "f64",
        };
        try std.fmt.format(writer, "{s}", .{val});
    }
};

pub const Value = union(ValType) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
};

pub const valtype = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    v128 = 0x7b,
    funcref = 0x70,
    externref = 0x6F,
    pub fn format(self: valtype, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const val: []const u8 = switch (self) {
            .i32 => "i32",
            .i64 => "i64",
            .f32 => "f32",
            .f64 => "f64",
            .v128 => "v128",
            .funcref => "funcref",
            .externref => "externref",
        };
        try std.fmt.format(writer, "{s}", .{val});
    }
};
pub const memarg = struct { @"align": u32, offset: u32 };
const UnOp = enum {
    clz,
    ctz,
    popcnt,
};

const BinOp = enum {
    add,
    sub,
    mul,
    div_s,
    div_u,
    rem_s,
    rem_u,
    @"and",
    @"or",
    xor,
    shl,
    shr_s,
    shr_u,
    rotl,
    rotr,
    pub fn format(self: BinOp, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const val = switch (self) {
            .add => "add",
            .sub => "sub",
            .mul => "mul",
            .div_s => "div_s",
            .div_u => "div_u",
            .rem_s => "rem_s",
            .rem_u => "rem_u",
            .@"and" => "and",
            .@"or" => "or",
            .xor => "xor",
            .shl => "shl",
            .shr_s => "shr_s",
            .shr_u => "shr_u",
            .rotl => "rotl",
            .rotr => "rotr",
        };
        try std.fmt.format(writer, "{s}", .{val});
    }
};

const FunOp = enum {
    abs,
    neg,
    sqrt,
    ceil,
    floor,
    trunc,
    nearest,
};

const FbinOp = enum {
    add,
    sub,
    mul,
    div,
    min,
    max,
    copysign,
};

const itestop = enum { eqz };

const irelop = enum {
    eq,
    ne,
    lt_s,
    lt_u,
    gt_s,
    gt_u,
    le_s,
    le_u,
    ge_s,
    ge_u,
};

const frelop = enum {
    eq,
    ne,
    lt,
    gt,
    le,
    ge,
};

pub const Instruction = union(enum) {
    constant: Value,
    local: struct { action: enum { get, set, tee }, idx: u32 },
    global: struct { action: enum { get, set }, idx: u32 },
    binOp: struct { val: ValType, op: BinOp },
    unOp: struct { val: ValType, op: UnOp },
    relOp: struct { val: ValType, op: irelop },
    block: struct { block_id: usize, block_type: enum { loop, block } },
    if_block: struct { @"if": usize, @"else": ?usize },
    load: struct { val: ValType, mem: memarg, extend: ?struct { width: u8, sign: enum { signed, unsigned } } = null },
    store: struct { val: ValType, mem: memarg, width: ?u8 = null },
    call: u32,

    unreach: void,
    pub fn format(self: Instruction, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .constant => |v| try std.fmt.format(writer, "{}", .{v}),
            .local => |l| try std.fmt.format(writer, "local.{s} {}", .{
                switch (l.action) {
                    .get => "get",
                    .set => "set",
                    .tee => "tee",
                },
                l.idx,
            }),
            .global => |g| try std.fmt.format(writer, "global.{s} {}", .{
                switch (g.action) {
                    .get => "get",
                    .set => "set",
                },
                g.idx,
            }),

            .binOp => |b| try std.fmt.format(writer, "{}.{}", .{ b.val, b.op }),
            //: struct { val: ValType, op: BinOp },
            //   unOp: struct { val: ValType, op: UnOp },

            //block: struct { block_id: usize, block_type: enum { loop, block } },
            //if_block: struct { @"if": usize, @"else": ?usize },
            .load => |l| try std.fmt.format(writer, "Load:({},{}", .{ l.val, l.mem }),
            //: struct { val: ValType, mem: memarg, extend: ?struct { width: u8, sign: enum { signed, unsigned } } = null },
            .store => |s| try std.fmt.format(writer, "Store({},{},{?})", .{ s.val, s.mem, s.width }),
            //: struct { val: ValType, mem: memarg, width: ?u8 = null },
            .call => |c| try std.fmt.format(writer, "Call({})", .{c}),

            .unreach => try std.fmt.format(writer, "unreachable", .{}),
            .unOp => |u| try std.fmt.format(writer, "UnOp({}:{})", .{ u.val, u.op }),
            .relOp => |r| try std.fmt.format(writer, "RelOp({}:{})", .{ r.val, r.op }),
            .if_block => |i| try std.fmt.format(writer, "IfBlock({}:{?})", .{ i.@"if", i.@"else" }),
            .block => |b| try std.fmt.format(writer, "Block({}:{s})", .{ b.block_id, @tagName(b.block_type) }),
        }
    }
};

pub const Block = struct {
    id: usize,
    ins: []const Instruction,
};

pub const Function = struct {
    params: []const valtype,
    ret: []const valtype,
    locals: []const valtype,
    blocks: []const Block,
};
const WasmFile = @import("WasmFile.zig");
pub const Module = struct {
    functions: []const Function,
    exports: []const WasmFile.Export,
};

pub const State = struct {
    stack: std.ArrayList(Value),

    pub fn init(alloc: std.mem.Allocator) State {
        return .{
            .stack = std.ArrayList(Value).init(alloc),
        };
    }
    pub fn deinit(self: State) void {
        self.stack.deinit();
    }
};

pub const ExecCtx = struct {
    module: Module,
    state: State,
    pub fn init(
        alloc: std.mem.Allocator,
        functions: []const Function,
        exports: []const WasmFile.Export,
    ) ExecCtx {
        return .{
            .module = .{
                .functions = functions,
                .exports = exports,
            },
            .state = State.init(alloc),
        };
    }
    pub fn deinit(self: ExecCtx) void {
        self.state.deinit();
    }
    pub fn exec(
        self: ExecCtx,
        name: []const u8,
        args: []const Value,
        results: *std.ArrayList(Value),
        alloc: std.mem.Allocator,
    ) !void {
        const f_idx = blk: {
            for (self.module.exports) |x| {
                if (std.mem.eql(u8, name, x.nm.data.items)) {
                    switch (x.desc) {
                        .func => |f| break :blk f,
                        else => {},
                    }
                }
            }
            return error.MissingFunction;
        };
        if (f_idx.idx >= self.module.functions.len) return error.InvalidFunction;
        const function = self.module.functions[f_idx.idx];
        var stack = Stack.init(alloc);
        defer stack.deinit();
        var locals = try Locals.init(alloc, args, function.locals);
        defer locals.deinit();
        var globals = Globals.init(alloc);
        defer globals.deinit();
        for (args, 0..) |a, idx| {
            std.log.info("[{}]{}", .{ idx, a });
        }
        for (function.locals, 0..) |l, idx| {
            std.log.info("[{}]{}", .{ idx, l });
        }
        for (function.blocks, 0..) |b, idx| {
            for (b.ins) |i| {
                std.log.info("[{}]{}", .{ idx, i });
            }
        }
        for (function.blocks[0].ins) |ins| {
            try exec_ins(
                ins,
                &stack,
                &locals,
                &globals,
            );
        }
        for (stack.items.items, 0..) |i, idx| {
            std.log.info("[{}]{}", .{ idx, i });
        }

        _ = results;
    }
};

pub const Stack = struct {
    items: std.ArrayList(Value),
    pub fn init(alloc: std.mem.Allocator) Stack {
        return .{
            .items = std.ArrayList(Value).init(alloc),
        };
    }
    pub fn deinit(self: Stack) void {
        self.items.deinit();
    }
    pub fn push(self: *Stack, val: Value) !void {
        try self.items.append(val);
    }
    pub fn pop(self: *Stack) !Value {
        if (self.items.pop()) |v| return v;
        return error.NotEnoughData;
    }
    pub fn peek(self: *Stack) !Value {
        if (self.items.items.len > 0) return self.items.items[self.items.items.len - 1];
        return error.NotEnoughData;
    }
};
pub const Locals = struct {
    items: std.ArrayList(?Value),
    types: []const valtype,
    pub fn init(alloc: std.mem.Allocator, params: []const Value, types: []const valtype) !Locals {
        var self = Locals{
            .items = std.ArrayList(?Value).init(alloc),
            .types = types,
        };
        errdefer self.deinit();
        for (params) |p| {
            try self.items.append(p);
        }
        try self.items.appendNTimes(null, types.len);
        return self;
    }
    pub fn deinit(self: Locals) void {
        self.items.deinit();
    }
    pub fn get(self: *Locals, idx: usize) !Value {
        if (idx >= self.items.items.len) return error.Invalid;
        if (self.items.items[idx]) |v| {
            return v;
        } else {
            return error.ReadingInvalidmemory; //Is this required? should it just return 0?
        }
    }
    pub fn set(self: *Locals, val: Value, idx: usize) !void {
        if (idx >= self.items.items.len) return error.Invalid;
        const exp = self.types[idx];
        switch (val) {
            .i32 => if (exp != .i32) return error.InvalidType,
            .i64 => if (exp != .i64) return error.InvalidType,
            .f32 => if (exp != .f32) return error.InvalidType,
            .f64 => if (exp != .f64) return error.InvalidType,
        }

        {
            return error.InvalidType;
        }
    }
};
pub const Globals = struct {
    items: std.ArrayList(Value),
    pub fn init(alloc: std.mem.Allocator) Globals {
        return .{
            .items = std.ArrayList(Value).init(alloc),
        };
    }
    pub fn deinit(self: Globals) void {
        self.items.deinit();
    }
};

fn exec_ins(
    ins: Instruction,
    stack: *Stack,
    locals: *Locals,
    globals: *Globals,
) !void {
    _ = globals;
    errdefer std.log.err("Failed to execute: {}", .{ins});
    switch (ins) {
        .constant => |v| try stack.push(v),
        .unOp => |u| try stack.push(try exec_unop(try stack.pop(), u.op)),
        .binOp => |b| try stack.push(try exec_binop(try stack.pop(), try stack.pop(), b.op)),
        .local => |l| {
            switch (l.action) {
                .get => try stack.push(try locals.get(l.idx)),
                .set => try locals.set(try stack.pop(), l.idx),
                .tee => try locals.set(try stack.peek(), l.idx),
            }
        },
        //.global => |g| {
        //switch (g.action) {
        //.get => try stack.push(try locals.get(g.idx)),
        //.set => try globals.set(try stack.pop(), g.idx),
        //}
        //},
        else => return error.TODO,
    }
}

fn bin_op(comptime T: type, op: BinOp, a: T, b: T) T {
    const is_int = @typeInfo(T) == .int;
    const bit_int = @Type(.{ .int = .{ .signedness = .unsigned, .bits = @bitSizeOf(T) } });
    return switch (op) {
        .add => a + b,
        .sub => a - b,
        .mul => a * b,
        .div_s => @divTrunc(a, b),
        .div_u => @divTrunc(a, b), //TODO
        .rem_s => @rem(a, b),
        .rem_u => @rem(a, b), //TODO
        .@"and" => if (is_int) a & b else 0,
        .@"or" => if (is_int) a | b else 0,
        .xor => if (is_int) a ^ b else 0,
        .shl => if (is_int) a << @truncate(@as(bit_int, @bitCast(b))) else 0,
        .shr_s => if (is_int) a >> @truncate(@as(bit_int, @bitCast(b))) else 0,
        .shr_u => if (is_int) a >> @truncate(@as(bit_int, @bitCast(b))) else 0, //TODO
        .rotl => 0,
        .rotr => 0,
    };
}

fn exec_binop(a: Value, b: Value, op: BinOp) !Value {
    switch (a) {
        .i32 => |v_a| {
            const v_b = switch (b) {
                .i32 => |v_b| v_b,
                else => return error.MixedVar,
            };
            return .{ .i32 = bin_op(i32, op, v_a, v_b) };
        },
        .i64 => |v_a| {
            const v_b = switch (b) {
                .i64 => |v_b| v_b,
                else => return error.MixedVar,
            };
            return .{ .i64 = bin_op(i64, op, v_a, v_b) };
        },
        .f32 => |v_a| {
            const v_b = switch (b) {
                .f32 => |v_b| v_b,
                else => return error.MixedVar,
            };
            return .{ .f32 = bin_op(f32, op, v_a, v_b) };
        },
        .f64 => |v_a| {
            const v_b = switch (b) {
                .f64 => |v_b| v_b,
                else => return error.MixedVar,
            };
            return .{ .f64 = bin_op(f64, op, v_a, v_b) };
        },
    }
    return .{ .i32 = 0 };
}

fn exec_unop(val: Value, op: UnOp) !Value {
    _ = val;
    _ = op;
    return .{ .i32 = 0 };
}
