const std = @import("std");
const Exec = @import("Exec.zig");
const Instr = enum(u8) {
    //control instructions
    unreach = 0x00,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    if_block = 0x04, // or if_else
    el_if = 0x05,

    end_block = 0x0B,
    branch = 0x0C,
    branch_if = 0x0D,
    branch_table = 0x0E,
    @"return" = 0x0F,

    call = 0x10,
    call_indirect = 0x11,

    //parametric
    drop = 0x1a,
    select = 0x1b,
    select_t = 0x1C,

    //variable
    local_get = 0x20,
    local_set = 0x21,
    local_tee = 0x22,
    global_get = 0x23,
    global_set = 0x24,

    //table
    table_get = 0x25,
    table_set = 0x26,

    //memory
    i32_load = 0x28,
    i64_load = 0x29,
    f32_load = 0x2A,
    f64_load = 0x2B,
    i32_load8_s = 0x2C,
    i32_load8_u = 0x2D,
    i32_load16_s = 0x2E,
    i32_load16_u = 0x2F,

    i64_load8_s = 0x30,
    i64_load8_u = 0x31,
    i64_load16_s = 0x32,
    i64_load16_u = 0x33,
    i64_load32_s = 0x34,
    i64_load32_u = 0x35,

    i32_store = 0x36,
    i64_store = 0x37,
    f32_store = 0x38,
    f64_store = 0x39,
    i32_store_8 = 0x3A,
    i32_store_16 = 0x3B,
    i64_store_8 = 0x3C,
    i64_store_16 = 0x3D,
    i64_store_32 = 0x3E,
    memory_size = 0x3f,
    memory_grow = 0x40,

    //constants
    i32_const = 0x41,
    i64_const = 0x42,
    f32_const = 0x43,
    f64_const = 0x44,

    //numerics

    i32_eqz = 0x45,
    i32_eq = 0x46,
    i32_ne = 0x47,
    i32_lt_s = 0x48,
    i32_lt_u = 0x49,
    i32_gt_s = 0x4A,
    i32_gt_u = 0x4B,
    i32_le_s = 0x4C,
    i32_le_u = 0x4D,
    i32_ge_s = 0x4E,
    i32_ge_u = 0x4F,

    i64_eqz = 0x50,
    i64_eq = 0x51,
    i64_ne = 0x52,
    i64_lt_s = 0x53,
    i64_lt_u = 0x54,
    i64_gt_s = 0x55,
    i64_gt_u = 0x56,
    i64_le_s = 0x57,
    i64_le_u = 0x58,
    i64_ge_s = 0x59,
    i64_ge_u = 0x5A,

    f32_eq = 0x5B,
    f32_ne = 0x5C,
    f32_lt = 0x5D,
    f32_gt = 0x5E,
    f32_le = 0x5F,
    f32_ge = 0x60,

    f64_eq = 0x61,
    f64_ne = 0x62,
    f64_lt = 0x63,
    f64_gt = 0x64,
    f64_le = 0x65,
    f64_ge = 0x66,

    i32_clz = 0x67,
    i32_ctz = 0x68,
    i32_popcnt = 0x69,
    i32_add = 0x6A,
    i32_sub = 0x6B,
    i32_mul = 0x6C,
    i32_div_s = 0x6D,
    i32_div_u = 0x6E,
    i32_rem_s = 0x6F,
    i32_rem_u = 0x70,
    i32_and = 0x71,
    i32_or = 0x72,
    i32_xor = 0x73,
    i32_shl = 0x74,
    i32_shr_s = 0x75,
    i32_shr_u = 0x76,
    i32_rotl = 0x77,
    i32_rotr = 0x78,

    i64_clz = 0x79,
    i64_ctz = 0x7A,
    i64_popcnt = 0x7B,
    i64_add = 0x7C,
    i64_sub = 0x7D,
    i64_mul = 0x7E,
    i64_div_s = 0x7F,
    i64_div_u = 0x80,
    i64_rem_s = 0x81,
    i64_rem_u = 0x82,
    i64_and = 0x83,
    i64_or = 0x84,
    i64_xor = 0x85,
    i64_shl = 0x86,
    i64_shr_s = 0x87,
    i64_shr_u = 0x88,
    i64_rotl = 0x89,
    i64_rotr = 0x8A,

    f32_abs = 0x8B,
    f32_neg = 0x8C,
    f32_ceil = 0x8D,
    f32_floor = 0x8E,
    f32_trunc = 0x8F,
    f32_nearest = 0x90,
    f32_sqrt = 0x91,
    f32_add = 0x92,
    f32_sub = 0x93,
    f32_mul = 0x94,
    f32_div = 0x95,
    f32_min = 0x96,
    f32_max = 0x97,
    f32_copysign = 0x98,

    f64_abs = 0x99,
    f64_neg = 0x9A,
    f64_ceil = 0x9B,
    f64_floor = 0x9C,
    f64_trunc = 0x9D,
    f64_nearest = 0x9E,
    f64_sqrt = 0x9F,
    f64_add = 0xA0,
    f64_sub = 0xA1,
    f64_mul = 0xA2,
    f64_div = 0xA3,
    f64_min = 0xA4,
    f64_max = 0xA5,
    f64_copysign = 0xA6,

    i32_wrap_i64 = 0xA7,
    i32_trunc_f32_s = 0xA8,
    i32_trunc_f32_u = 0xA9,
    i32_trunc_f64_s = 0xAA,
    i32_trunc_f64_u = 0xAB,
    i64_extend_i32_s = 0xAC,
    i64_extend_i32_u = 0xAD,
    i64_trunc_f32_s = 0xAE,
    i64_trunc_f32_u = 0xAF,

    i64_trunc_f64_s = 0xB0,
    i64_trunc_f64_u = 0xB1,
    f32_convert_i32_s = 0xB2,
    f32_convert_i32_u = 0xB3,
    f32_convert_i64_s = 0xB4,
    f32_convert_i64_u = 0xB5,

    f32_demote_f64 = 0xB6,
    f64_convert_i32_s = 0xB7,
    f64_convert_i32_u = 0xB8,
    f64_convert_i64_s = 0xB9,
    f64_convert_i64_u = 0xBA,
    f64_promote_f32 = 0xBB,

    i32_reinterpret_f32 = 0xBC,
    i64_reinterpret_f64 = 0xBD,
    f32_reinterpret_i32 = 0xBE,
    f64_reinterpret_i64 = 0xBF,

    i32_extend8_s = 0xC0,
    i32_extend16_s = 0xC1,
    i64_extend8_s = 0xC2,
    i64_extend16_s = 0xC3,
    i64_extend32_s = 0xC4,

    //reference
    ref_null = 0xD0,
    ref_is_null = 0xD1,
    ref_func = 0xD2,

    extendedIns = 0xFC,
    vectorIns = 0xFD,
};

const extendedIns = enum(u32) {
    //numeric
    i32_trunc_sat_f32_s = 0,
    i32_trunc_sat_f32_u = 1,
    i32_trunc_sat_f64_s = 2,
    i32_trunc_sat_f64_u = 3,
    i64_trunc_sat_f32_s = 4,
    i64_trunc_sat_f32_u = 5,
    i64_trunc_sat_f64_s = 6,
    i64_trunc_sat_f64_u = 7,
    //memory
    memory_init = 8,
    data_drop = 9,
    memory_copy = 10,
    memory_fill = 11,
    //table
    table_init = 12,
    elem_drop = 13,
    table_copy = 14,
    table_grow = 15,
    table_size = 16,
    table_fill = 17,
};

const VectorIns = enum(u32) {
    v128_load = 0,
    v128_load8x8_s = 1,
    v128_load8x8_u = 2,
    v128_load16x4_s = 3,
    v128_load16x4_u = 4,

    v128_load32x2_s = 5,
    v128_load32x2_u = 6,
    v128_load8_splat = 7,
    v128_load16_splat = 8,
    v128_load32_splat = 9,
    v128_load64_splat = 10,
    v128_store = 11,

    v128_const_bytes = 12,
    i8x16_shuffle = 13,

    i8x16_swizzle = 14,
    i8x16_splat = 15,
    i16x8_splat = 16,
    i32x4_splat = 17,
    i64x2_splat = 18,
    f32x4_splat = 19,
    f64x2_splat = 19,

    i8x16_extract_lane_s = 21,
    i8x16_extract_lane_u = 22,
    i8x16_replace_lane = 23,

    i16x8_extract_lane_s = 24,
    i16x8_extract_lane_u = 25,
    i16x8_replace_lane = 26,

    i32x4_extract_lane = 27,
    i32x4_replace_lane = 28,

    i64x2_extract_lane = 29,
    i64x2_replace_lane = 30,

    f32x4_extract_lane = 31,
    f32x4_replace_lane = 32,

    f64x2_extract_lane = 33,
    f64x2_replace_lane = 34,

    v128_load8_lane = 84,
    v128_load16_lane = 85,
    v128_load32_lane = 86,
    v128_load64_lane = 87,

    v128_store8_lane = 88,
    v128_store16_lane = 89,
    v128_store32_lane = 90,
    v128_store64_lane = 91,

    v128_load32_zero = 92,
    v128_load64_zero = 93,
    //TOOD still needs more vector
};

const Reader = @import("WasmFile.zig").Reader;
const Value = Exec.Value;
const ValType = Exec.ValType;

const Local = struct {
    idx: u32,
    action: Action,
    const Action = enum { get, set, tee };
};

const Global = struct {
    idx: u32,
    action: Action,
    const Action = enum { get, set };
};

const Instruction = union(enum) {
    value: Value,
    local: Local,
    global: Global,

    loop: struct { block_id: usize },
    block: struct { block_id: usize },

    numeric: Numeric,
    //load: struct { type: ValType, arg: memarg },
    branch: struct { label: u32, conditional: bool },
    call: union(enum) { direct: u32, indirect: struct { type: u32, table: u32 } },
};

pub const Numeric = struct {
    type: ValType,
    op: Op,
};

const Op = enum {
    eqz,
    eq,
    ne,
    lt_s,
    gt_s,
    le_s,
    ge_s,
    lt_u,
    gt_u,
    le_u,
    ge_u,
    mul,
    add,
    sub,
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
    clz,
    ctz,
    popcnt,
};

pub const Function = struct {
    alloc: std.mem.Allocator,
    blocks: std.ArrayList(*Block),
    pub const Block = struct {
        id: usize,
        ins: std.ArrayList(Exec.Instruction),
        pub fn init(alloc: std.mem.Allocator, id: usize) Block {
            return .{
                .id = id,
                .ins = std.ArrayList(Exec.Instruction).init(alloc),
            };
        }
    };
    pub fn init(alloc: std.mem.Allocator) Function {
        return .{
            .alloc = alloc,
            .blocks = std.ArrayList(*Block).init(alloc),
        };
    }
    pub fn deinit(self: Function) void {
        for (self.blocks.items) |i| {
            i.ins.deinit();
            self.alloc.destroy(i);
        }
        self.blocks.deinit();
    }
    pub fn new_block(self: *Function) !*Block {
        const bl = try self.alloc.create(Block);
        errdefer self.alloc.destroy(bl);
        const id = self.blocks.items.len;
        bl.* = Block.init(self.alloc, id);
        try self.blocks.append(bl);
        return bl;
    }
};
pub fn parseExp(reader: *Reader, func: *Function) !struct {
    block_id: usize,
    block_type: enum { else_if, block_end },
} {
    const bl = try func.new_block();
    std.log.err("ParseExp|{}", .{std.fmt.fmtSliceHexUpper(reader.data[reader.idx..])});

    while (true) {
        const tag = try reader.readEnum(Instr);

        const val: Exec.Instruction = switch (tag) {
            .i32_const => .{ .constant = .{ .i32 = try reader.read(i32) } },
            .i64_const => .{ .constant = .{ .i64 = try reader.read(i64) } },
            .f32_const => .{ .constant = .{ .f32 = try reader.read(f32) } },
            .f64_const => .{ .constant = .{ .f64 = try reader.read(f64) } },
            .end_block => return .{ .block_id = bl.id, .block_type = .block_end },
            .unreach => .unreach,
            .local_get => .{ .local = .{ .action = .get, .idx = try reader.read(u32) } },
            .local_set => .{ .local = .{ .action = .set, .idx = try reader.read(u32) } },
            .local_tee => .{ .local = .{ .action = .tee, .idx = try reader.read(u32) } },
            .global_get => .{ .global = .{ .action = .get, .idx = try reader.read(u32) } },
            .global_set => .{ .global = .{ .action = .set, .idx = try reader.read(u32) } },
            .loop => blk: {
                const bt = try reader.read(u8);
                std.log.err("{}", .{bt});
                const block = try parseExp(reader, func);

                break :blk .{ .block = .{ .block_id = block.block_id, .block_type = .loop } };
            },
            .block => blk: {
                const bt = try reader.read(u8);
                std.log.err("{}", .{bt});
                const block = try parseExp(reader, func);
                break :blk .{ .block = .{ .block_id = block.block_id, .block_type = .block } };
            },
            .if_block => blk: {
                const bt = try reader.read(u8);
                _ = bt;
                const block = try parseExp(reader, func);
                const else_block = switch (block.block_type) {
                    .block_end => null,
                    .else_if => (try parseExp(reader, func)).block_id,
                };
                break :blk .{ .if_block = .{ .@"if" = block.block_id, .@"else" = else_block } };
            },
            .el_if => return .{ .block_id = bl.id, .block_type = .else_if },
            .i32_load => .{ .load = .{ .val = .i32, .mem = try reader.read(Exec.memarg) } },
            .i64_load => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg) } },
            .f32_load => .{ .load = .{ .val = .f32, .mem = try reader.read(Exec.memarg) } },
            .f64_load => .{ .load = .{ .val = .f64, .mem = try reader.read(Exec.memarg) } },
            .i32_load8_s => .{ .load = .{ .val = .i32, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 1, .sign = .signed } } },
            .i32_load8_u => .{ .load = .{ .val = .i32, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 1, .sign = .unsigned } } },
            .i32_load16_s => .{ .load = .{ .val = .i32, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 2, .sign = .signed } } },
            .i32_load16_u => .{ .load = .{ .val = .i32, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 2, .sign = .unsigned } } },

            .i64_load8_s => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 1, .sign = .signed } } },
            .i64_load8_u => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 1, .sign = .unsigned } } },
            .i64_load16_s => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 2, .sign = .signed } } },
            .i64_load16_u => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 2, .sign = .unsigned } } },
            .i64_load32_s => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 4, .sign = .signed } } },
            .i64_load32_u => .{ .load = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .extend = .{ .width = 4, .sign = .unsigned } } },

            .i32_store => .{ .store = .{ .val = .i32, .mem = try reader.read(Exec.memarg) } },
            .i64_store => .{ .store = .{ .val = .i64, .mem = try reader.read(Exec.memarg) } },
            .f32_store => .{ .store = .{ .val = .f32, .mem = try reader.read(Exec.memarg) } },
            .f64_store => .{ .store = .{ .val = .f64, .mem = try reader.read(Exec.memarg) } },
            .i32_store_8 => .{ .store = .{ .val = .i32, .mem = try reader.read(Exec.memarg), .width = 1 } },
            .i32_store_16 => .{ .store = .{ .val = .i32, .mem = try reader.read(Exec.memarg), .width = 2 } },

            .i64_store_8 => .{ .store = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .width = 1 } },
            .i64_store_16 => .{ .store = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .width = 2 } },
            .i64_store_32 => .{ .store = .{ .val = .i64, .mem = try reader.read(Exec.memarg), .width = 4 } },

            // .i32_eqz => try bl.ins.append(.{.binOp }),
            .i32_eq => .{ .relOp = .{ .val = .i32, .op = .eq } },
            .i32_ne => .{ .relOp = .{ .val = .i32, .op = .ne } },
            .i32_lt_s => .{ .relOp = .{ .val = .i32, .op = .lt_s } },
            .i32_lt_u => .{ .relOp = .{ .val = .i32, .op = .lt_u } },
            .i32_gt_s => .{ .relOp = .{ .val = .i32, .op = .gt_s } },
            .i32_gt_u => .{ .relOp = .{ .val = .i32, .op = .gt_u } },
            .i32_le_s => .{ .relOp = .{ .val = .i32, .op = .le_s } },
            .i32_le_u => .{ .relOp = .{ .val = .i32, .op = .le_u } },
            .i32_ge_s => .{ .relOp = .{ .val = .i32, .op = .ge_s } },
            .i32_ge_u => .{ .relOp = .{ .val = .i32, .op = .ge_u } },

            // .i64_lt_s => .{ .numeric = .{ .type = .i64, .op = .lt_s } },
            // .i64_lt_u => .{ .numeric = .{ .type = .i64, .op = .lt_u } },
            // .i64_gt_s => .{ .numeric = .{ .type = .i64, .op = .gt_s } },
            // .i64_gt_u => .{ .numeric = .{ .type = .i64, .op = .gt_u } },
            // .i64_le_s => .{ .numeric = .{ .type = .i64, .op = .le_s } },
            // .i64_le_u => .{ .numeric = .{ .type = .i64, .op = .le_u } },
            // .i64_ge_s => .{ .numeric = .{ .type = .i64, .op = .ge_s } },
            // .i64_ge_u => .{ .numeric = .{ .type = .i64, .op = .ge_u } },

            // .i32_clz => .{ .numeric = .{ .type = .i32, .op = .clz } },
            // .i32_ctz => .{ .numeric = .{ .type = .i32, .op = .ctz } },
            // .i32_popcnt => .{ .numeric = .{ .type = .i32, .op = .popcnt } },
            .i32_add => .{ .binOp = .{ .val = .i32, .op = .add } },
            .i32_sub => .{ .binOp = .{ .val = .i32, .op = .sub } },
            .i32_mul => .{ .binOp = .{ .val = .i32, .op = .mul } },
            .i32_div_s => .{ .binOp = .{ .val = .i32, .op = .div_s } },
            .i32_div_u => .{ .binOp = .{ .val = .i32, .op = .div_u } },
            .i32_rem_s => .{ .binOp = .{ .val = .i32, .op = .rem_s } },
            .i32_rem_u => .{ .binOp = .{ .val = .i32, .op = .rem_u } },
            .i32_and => .{ .binOp = .{ .val = .i32, .op = .@"and" } },
            .i32_or => .{ .binOp = .{ .val = .i32, .op = .@"or" } },
            .i32_xor => .{ .binOp = .{ .val = .i32, .op = .xor } },
            .i32_shl => .{ .binOp = .{ .val = .i32, .op = .shl } },
            .i32_shr_s => .{ .binOp = .{ .val = .i32, .op = .shr_s } },
            .i32_shr_u => .{ .binOp = .{ .val = .i32, .op = .shr_u } },
            .i32_rotl => .{ .binOp = .{ .val = .i32, .op = .rotl } },
            .i32_rotr => .{ .binOp = .{ .val = .i32, .op = .rotr } },

            .i64_add => .{ .binOp = .{ .val = .i64, .op = .add } },
            .i64_sub => .{ .binOp = .{ .val = .i64, .op = .sub } },
            .i64_mul => .{ .binOp = .{ .val = .i64, .op = .mul } },
            .i64_div_s => .{ .binOp = .{ .val = .i64, .op = .div_s } },
            .i64_div_u => .{ .binOp = .{ .val = .i64, .op = .div_u } },
            .i64_rem_s => .{ .binOp = .{ .val = .i64, .op = .rem_s } },
            .i64_rem_u => .{ .binOp = .{ .val = .i64, .op = .rem_u } },
            .i64_and => .{ .binOp = .{ .val = .i64, .op = .@"and" } },
            .i64_or => .{ .binOp = .{ .val = .i64, .op = .@"or" } },
            .i64_xor => .{ .binOp = .{ .val = .i64, .op = .xor } },
            .i64_shl => .{ .binOp = .{ .val = .i64, .op = .shl } },
            .i64_shr_s => .{ .binOp = .{ .val = .i64, .op = .shr_s } },
            .i64_shr_u => .{ .binOp = .{ .val = .i64, .op = .shr_u } },
            .i64_rotl => .{ .binOp = .{ .val = .i64, .op = .rotl } },
            .i64_rotr => .{ .binOp = .{ .val = .i64, .op = .rotr } },

            // .branch => .{ .branch = .{ .label = try reader.read(u32), .conditional = false } },
            // .branch_if => .{ .branch = .{ .label = try reader.read(u32), .conditional = true } },

            .call => .{ .call = try reader.read(u32) },
            // .call_indirect => .{ .call = .{ .indirect = .{ .type = try reader.read(u32), .table = try reader.read(u32) } } },

            else => {
                std.log.err("{}", .{tag});
                return error.TODO;
            },
        };
        try bl.ins.append(val);

        std.log.err("{}", .{val});
    }
    return bl.id;
}
