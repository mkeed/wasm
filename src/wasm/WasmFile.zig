const std = @import("std");
const Exec = @import("Exec.zig");

pub const typeidx = struct { idx: u32 };
pub const funcidx = struct { idx: u32 };
pub const tableidx = struct { idx: u32 };
pub const memidx = struct { idx: u32 };
pub const globalidx = struct { idx: u32 };
pub const elemidx = struct { idx: u32 };
pub const dataidx = struct { idx: u32 };
pub const localidx = struct { idx: u32 };
pub const labelidx = struct { idx: u32 };

pub const limit = struct {
    min: u32,
    max: ?u32,
    pub fn decode(reader: *Reader, alloc: std.mem.Allocator) !limit {
        const tag = try decoder(u8, reader, alloc);
        return .{
            .min = try decoder(u32, reader, alloc),
            .max = if (tag == 0x01) try decoder(u32, reader, alloc) else null,
        };
    }
};

pub const tabletype = struct {
    et: reftype,
    lim: limit,
};

pub const memtype = struct {
    lim: limit,
};

pub const mut = enum(u8) {
    @"const" = 0,
    @"var" = 1,
};

pub const globaltype = struct {
    t: valtype,
    m: mut,
};

pub const SectionType = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    @"export" = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
};

pub const numtype = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    pub fn format(self: numtype, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const val: []const u8 = switch (self) {
            .i32 => "i32",
            .i64 => "i64",
            .f32 => "f32",
            .f64 => "f64",
        };
        try std.fmt.format(writer, "{s}", .{val});
    }
};

pub const vectype = enum(u8) {
    v128 = 0x7b,
    pub fn format(self: vectype, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const val: []const u8 = switch (self) {
            .v128 => "v128",
        };
        try std.fmt.format(writer, "{s}", .{val});
    }
};

pub const reftype = enum(u8) {
    funcref = 0x70,
    externref = 0x6F,
    pub fn format(self: reftype, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const val: []const u8 = switch (self) {
            .funcref => "funcref",
            .externref => "externref",
        };
        try std.fmt.format(writer, "{s}", .{val});
    }
};

const valtype = Exec.valtype;

const resulttype = struct {
    vals: Vec(valtype),
    pub fn init(alloc: std.mem.Allocator) resulttype {
        return .{
            .vals = Vec(valtype).init(alloc),
        };
    }
    pub fn deinit(self: resulttype) void {
        self.vals.deinit();
    }
};
const functype = struct {
    pub fn decode(reader: *Reader, alloc: std.mem.Allocator) !functype {
        if ((try reader.readByte()) != 0x60) return error.InvalidMagicNumber;
        const rt1 = try decoder(resulttype, reader, alloc);
        errdefer rt1.vals.deinit();
        const rt2 = try decoder(resulttype, reader, alloc);
        errdefer rt2.vals.deinit();
        return .{
            .rt1 = rt1,
            .rt2 = rt2,
        };
    }
    rt1: resulttype,
    rt2: resulttype,
    pub fn deinit(self: functype) void {
        self.rt1.deinit();
        self.rt2.deinit();
    }
    pub fn format(self: functype, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "((rt1 ({})) (rt2 ({})))\n", .{ self.rt1.vals, self.rt2.vals });
    }
};

const TypeSection = struct {
    types: Vec(functype),
    pub fn init(alloc: std.mem.Allocator) TypeSection {
        return .{
            .vals = Vec(TypeSection).init(alloc),
        };
    }
    pub fn deinit(self: TypeSection) void {
        self.types.deinit();
    }
};

fn Vec(comptime T: type) type {
    return struct {
        const Self = @This();
        vals: std.ArrayList(T),
        pub fn init(alloc: std.mem.Allocator) Self {
            return .{
                .vals = std.ArrayList(T).init(alloc),
            };
        }
        pub fn decode(reader: *Reader, alloc: std.mem.Allocator) !Self {
            var self = Self.init(alloc);
            errdefer self.deinit();
            const len = try decoder(u32, reader, alloc);
            try self.vals.ensureTotalCapacity(len);
            for (0..len) |_| {
                try self.vals.append(try decoder(T, reader, alloc));
            }
            return self;
        }
        pub fn deinit(self: Self) void {
            switch (@typeInfo(T)) {
                .@"struct", .@"union" => {
                    if (@hasDecl(T, "deinit")) {
                        for (self.vals.items) |i| i.deinit();
                    }
                },
                else => {},
            }
            self.vals.deinit();
        }
        pub fn format(self: Self, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            for (self.vals.items) |val| {
                try std.fmt.format(writer, "({}) ", .{val});
            }
        }
    };
}

pub const name = struct {
    data: std.ArrayList(u8),
    pub fn init(alloc: std.mem.Allocator) name {
        return .{
            .data = std.ArrayList(u8).init(alloc),
        };
    }
    pub fn deinit(self: name) void {
        self.data.deinit();
    }
    pub fn decode(reader: *Reader, alloc: std.mem.Allocator) !name {
        var self = name.init(alloc);
        errdefer self.deinit();
        const len = try decoder(u32, reader, alloc);
        const bytes = try reader.readBytes(len);
        try self.data.appendSlice(bytes);
        return self;
    }
    pub fn format(self: name, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{s}", .{self.data.items});
    }
};

pub const ImportSection = Vec(import);

pub const import = struct {
    mod: name,
    nm: name,
    desc: importdesc,
    pub fn deinit(self: import) void {
        self.mod.deinit();
        self.nm.deinit();
    }
};
pub const importdescenum = enum(u8) {
    func = 0,
    table = 1,
    mem = 2,
    global = 3,
};
pub const importdesc = union(importdescenum) {
    func: typeidx,
    table: tabletype,
    mem: memtype,
    global: globaltype,
    //
};

const FunctionSection = Vec(typeidx);
const TableSection = struct {};
const MemorySection = struct {};
const GlobalSection = struct {};
const ExportSection = Vec(Export);
pub const Export = struct {
    nm: name,
    desc: exportdesc,

    const exportenum = enum(u8) {
        func = 0,
        table = 1,
        mem = 2,
        global = 3,
    };

    pub const exportdesc = union(exportenum) {
        func: funcidx,
        table: tableidx,
        mem: memidx,
        global: globalidx,
    };

    pub fn deinit(self: Export) void {
        self.nm.deinit();
    }
};
const StartSection = struct {};
const ElementSection = struct {};
const CodeSection = Vec(func);
const func = struct {
    const Expr = @import("Expr.zig");
    locals: Vec(Locals),
    func: Expr.Function,
    pub fn decode(reader: *Reader, alloc: std.mem.Allocator) !func {
        const size = try decoder(u32, reader, alloc);
        var sub = try reader.subReader(size);
        const locals = try decoder(Vec(Locals), &sub, alloc);
        errdefer locals.deinit();
        std.log.err("Code:[{}]", .{std.fmt.fmtSliceHexUpper(sub.data[sub.idx..])});

        var function = Expr.Function.init(alloc);
        errdefer function.deinit();
        _ = try Expr.parseExp(&sub, &function);
        return func{
            .locals = locals,
            .func = function,
        };
    }
    pub fn deinit(self: func) void {
        self.locals.deinit();
        self.func.deinit();
    }
    pub fn format(self: func, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "({})\n", .{self.locals});
        for (self.func.blocks.items) |b| {
            for (b.ins.items) |ins| {
                try std.fmt.format(writer, "{}\n", .{ins});
            }
        }
    }
    const Locals = struct {
        n: u32,
        t: valtype,
    };
    pub const expr = struct {};
};
const DataSection = struct {};
const DataCountSection = struct {};

const SectionData = struct {
    data: []const u8,
};

pub const WasmFile = struct {
    custom: ?SectionData = null,
    type: ?SectionData = null,
    import: ?SectionData = null,
    function: ?SectionData = null,
    table: ?SectionData = null,
    memory: ?SectionData = null,
    global: ?SectionData = null,
    @"export": ?SectionData = null,
    start: ?SectionData = null,
    element: ?SectionData = null,
    code: ?SectionData = null,
    data: ?SectionData = null,
    data_count: ?SectionData = null,
};

pub fn decoder(comptime T: type, reader: *Reader, alloc: std.mem.Allocator) !T {
    switch (@typeInfo(T)) {
        .int => |i| {
            switch (i.signedness) {
                .signed => return try std.leb.readIleb128(T, reader),
                .unsigned => return try std.leb.readUleb128(T, reader),
            }
        },
        .float => {
            @compileError("TODO FLoats");
        },
        .@"enum" => |e| {
            const tag = try decoder(e.tag_type, reader, alloc);
            return std.meta.intToEnum(T, tag);
        },
        .@"union" => |u| {
            const tag = try decoder(u.tag_type.?, reader, alloc);
            inline for (u.fields) |uf| {
                if (std.mem.eql(u8, uf.name, @tagName(tag))) {
                    return @unionInit(T, uf.name, try decoder(uf.type, reader, alloc));
                }
            }
            unreachable;
        },
        .@"struct" => |s| {
            if (@hasDecl(T, "decode")) {
                return try T.decode(reader, alloc);
            } else {
                var ret: T = undefined;
                inline for (s.fields) |sf| {
                    @field(ret, sf.name) = try decoder(sf.type, reader, alloc);
                }
                return ret;
            }
        },
        else => {
            @compileError("Invalid Option");
        },
    }
}

pub const Reader = struct {
    data: []const u8,
    idx: usize = 0,
    pub fn readByte(self: *Reader) !u8 {
        if (self.idx >= self.data.len) return error.EOF;
        defer self.idx += 1;
        return self.data[self.idx];
    }
    pub fn readBytes(self: *Reader, len: usize) ![]const u8 {
        if (self.idx + len > self.data.len) return error.TooLong;
        defer self.idx += len;
        return self.data[self.idx..][0..len];
    }
    pub fn readEnum(self: *Reader, comptime T: type) !T {
        const i = @typeInfo(T).@"enum";
        const tag = try self.read(i.tag_type);
        errdefer std.log.err("{}[{x}]", .{ tag, tag });
        return try std.meta.intToEnum(T, tag);
    }
    pub fn read(self: *Reader, comptime T: type) !T {
        if (T == u8) {
            return try self.readByte();
        }
        switch (@typeInfo(T)) {
            .int => |i| {
                switch (i.signedness) {
                    .signed => return try std.leb.readIleb128(T, self),
                    .unsigned => return try std.leb.readUleb128(T, self),
                }
            },
            .float => {
                const bytes = try self.readBytes(@sizeOf(T));
                const val: *const T = @alignCast(@ptrCast(bytes.ptr));
                //todo this is probably wrong
                return val.*;
            },
            .@"struct" => |s| {
                if (@hasDecl(T, "decode") or @hasDecl(T, "deinit")) {
                    @compileError("Complex Struct");
                }
                var ret: T = undefined;
                inline for (s.fields) |sf| {
                    @field(ret, sf.name) = try self.read(sf.type);
                }
                return ret;
            },
            else => {
                @compileError("Invalid request");
            },
        }
    }
    pub fn subReader(self: *Reader, len: u32) !Reader {
        return .{
            .data = try self.readBytes(len),
        };
    }
};

test {
    const files = [_]struct { name: []const u8, data: []const u8 }{
        //.{ .name = "mked", .data = @embedFile("mked.wasm") },
        //.{ .name = "add-not-folded.wasm", .data = @embedFile("add-not-folded.wasm") },
        .{ .name = "add.wasm", .data = @embedFile("add.wasm") },
        //.{ .name = "endianflip.wasm", .data = @embedFile("endianflip.wasm") },
        //.{ .name = "envprint.wasm", .data = @embedFile("envprint.wasm") },
        //.{ .name = "ifexpr.wasm", .data = @embedFile("ifexpr.wasm") },
        // .{ .name = "isprime.wasm", .data = @embedFile("isprime.wasm") },
        // .{ .name = "itoa.wasm", .data = @embedFile("itoa.wasm") },
        // .{ .name = "locals.wasm", .data = @embedFile("locals.wasm") },
        // .{ .name = "loops.wasm", .data = @embedFile("loops.wasm") },
        // .{ .name = "memory-basics.wasm", .data = @embedFile("memory-basics.wasm") },
        // .{ .name = "mod1.wasm", .data = @embedFile("mod1.wasm") },
        // .{ .name = "mod2.wasm", .data = @embedFile("mod2.wasm") },
        // .{ .name = "readfile.wasm", .data = @embedFile("readfile.wasm") },
        // .{ .name = "recursion.wasm", .data = @embedFile("recursion.wasm") },
        // .{ .name = "select.wasm", .data = @embedFile("select.wasm") },
        // .{ .name = "stack.wasm", .data = @embedFile("stack.wasm") },
        // .{ .name = "table.wasm", .data = @embedFile("table.wasm") },
        // .{ .name = "vcount.wasm", .data = @embedFile("vcount.wasm") },
        // .{ .name = "vecadd.wasm", .data = @embedFile("vecadd.wasm") },
        // .{ .name = "write.wasm", .data = @embedFile("write.wasm") },
    };
    for (files) |wasm| {
        errdefer std.log.err("File:{s}", .{wasm.name});
        //const wasm = @embedFile("wasm-wat-samples/add/add.wasm");
        const val = try decodeFile(wasm.data, std.testing.allocator);
        defer val.deinit();
    }
}

fn breakOutFile(data: []const u8) !WasmFile {
    var reader = Reader{ .data = data };
    _ = try reader.readBytes(4); //\0wasm
    _ = try reader.readBytes(4); //00 00 00 01 version
    var file = WasmFile{};
    while (reader.idx < reader.data.len) {
        const sec = try std.meta.intToEnum(SectionType, try reader.read(u8));
        const len = try reader.read(u32);
        const sub = try reader.subReader(len);
        std.log.err("{s}=>{} [{}]", .{ @tagName(sec), len, std.fmt.fmtSliceHexUpper(sub.data) });
        switch (sec) {
            .custom => file.custom = .{ .data = sub.data },
            .type => file.type = .{ .data = sub.data },

            .import => file.import = .{ .data = sub.data },
            .function => file.function = .{ .data = sub.data },
            .table => file.table = .{ .data = sub.data },
            .memory => file.memory = .{ .data = sub.data },
            .global => file.global = .{ .data = sub.data },
            .@"export" => file.@"export" = .{ .data = sub.data },
            .start => file.start = .{ .data = sub.data },
            .element => file.element = .{ .data = sub.data },
            .code => file.code = .{ .data = sub.data },
            .data => file.data = .{ .data = sub.data },
            .data_count => file.data_count = .{ .data = sub.data },
        }
    }
    return file;
}

fn decodeSection(comptime T: type, data: ?SectionData, alloc: std.mem.Allocator) !T {
    if (data) |d| {
        var reader = Reader{ .data = d.data };
        return try decoder(T, &reader, alloc);
    } else {
        return T.init(alloc);
    }
}

pub fn decodeFile(data: []const u8, alloc: std.mem.Allocator) !WasmModule {
    var arena = std.heap.ArenaAllocator.init(alloc);
    errdefer arena.deinit();
    const aa = arena.allocator();
    const File = try breakOutFile(data);
    const func_types = try decodeSection(Vec(functype), File.type, aa);
    const funcs = try decodeSection(FunctionSection, File.function, aa);
    const exports = try decodeSection(ExportSection, File.@"export", aa);
    const code = try decodeSection(CodeSection, File.code, aa);

    for (exports.vals.items) |i| {
        std.log.err("{s} {}]", .{ i.nm, i.desc });
    }

    const functions = try aa.alloc(Exec.Function, code.vals.items.len);

    for (code.vals.items, funcs.vals.items, 0..) |c, f_t, idx| {
        const ft = func_types.vals.items[f_t.idx];
        const blocks = try aa.alloc(Exec.Block, c.func.blocks.items.len);
        for (blocks, c.func.blocks.items) |*b, p| {
            b.* = .{
                .id = p.id,
                .ins = p.ins.items,
            };
            for (p.ins.items) |ins| {
                std.log.info("aa:{}", .{ins});
            }
        }
        var locals = std.ArrayList(valtype).init(aa);
        for (c.locals.vals.items) |l| {
            std.log.info("Adding locals:{}", .{l});
            try locals.appendNTimes(l.t, l.n);
        }
        functions[idx] = .{
            .params = ft.rt1.vals.vals.items,
            .ret = ft.rt2.vals.vals.items,
            .locals = locals.items,
            .blocks = blocks,
        };
        std.log.err("{}", .{c});
    }
    return .{
        .arena = arena,
        .functions = functions,
        .exports = exports.vals.items,
    };
}

pub const WasmModule = struct {
    arena: std.heap.ArenaAllocator,
    functions: []const Exec.Function,
    exports: []const Export,
    pub fn deinit(self: WasmModule) void {
        self.arena.deinit();
    }

    pub fn instantiate(self: WasmModule, alloc: std.mem.Allocator) Exec.ExecCtx {
        return Exec.ExecCtx.init(alloc, self.functions, self.exports);
    }
};
