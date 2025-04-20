const std = @import("std");

pub const Token = union(enum) {
    open: []const u8,
    close: void,
    token: []const u8,
    pub fn format(self: Token, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .open => |o| try std.fmt.format(writer, "Open:{s}", .{o}),
            .close => try std.fmt.format(writer, "Close", .{}),
            .token => |t| try std.fmt.format(writer, "Token[{s}]", .{t}),
        }
    }
};

pub const TokenIter = struct {
    data: []const u8,
    idx: usize = 0,
    fn get(self: *TokenIter) ?u8 {
        if (self.idx <= self.data.len) {
            defer self.idx += 1;
            return self.data[self.idx];
        } else {
            return null;
        }
    }
    fn peek(self: TokenIter) ?u8 {
        if (self.idx <= self.data.len) {
            return self.data[self.idx];
        } else {
            return null;
        }
    }
    pub fn next(self: *TokenIter) !?Token {
        while (self.get()) |ch| {
            switch (ch) {
                '(' => {
                    const start = self.idx;
                    while (self.peek()) |n| {
                        if (std.ascii.isWhitespace(n)) {
                            return .{
                                .open = self.data[start..self.idx],
                            };
                        }
                        _ = self.get();
                    }

                    return error.UnkownIssue;
                },
                ')' => return .close,
                ';' => {
                    if (self.get()) |n| {
                        if (n == ';') {
                            while (self.get()) |val| {
                                if (val == '\n') {
                                    break;
                                }
                            }
                        } else {
                            return error.IsThisAnError;
                        }
                    }
                },
                ' ', '\t'...'\r' => {},
                '"' => {
                    const start = self.idx;
                    while (self.get()) |n| {
                        if (n == '"') {
                            return .{
                                .token = self.data[start .. self.idx - 1],
                            };
                        }
                    }
                },
                else => {
                    const start = self.idx - 1;
                    while (self.peek()) |n| {
                        if (std.ascii.isWhitespace(n) or n == '(' or n == ')') {
                            return .{
                                .token = self.data[start..self.idx],
                            };
                        }
                        _ = self.get();
                    }
                },
            }
        }
        return null;
    }
};
