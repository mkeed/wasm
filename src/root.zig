const sexp = @import("wasm/sexp.zig");
const WasmFile = @import("wasm/WasmFile.zig");
const Exec = @import("wasm/Exec.zig");
pub const Value = Exec.Value;
pub fn compileModule(
    data: []const u8,
    alloc: std.mem.Allocator,
) !WasmFile.WasmModule {
    return WasmFile.decodeFile(data, alloc);
}

test {
    const wat = @embedFile("wasm/wasm-wat-samples/loops/loops.wat");
    var iter = sexp.TokenIter{ .data = wat };
    errdefer std.log.err("{s}", .{iter.data[iter.idx..]});
    while (try iter.next()) |t| {
        std.log.err("{}", .{t});
    }
}
const std = @import("std");
