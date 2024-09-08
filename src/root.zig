const std = @import("std");

/// Describes how a single struct field is encoded. For 'repeated' fields, this specifies whether
/// the values are packed; otherwise it just tells us the representation of the actual type (e.g.
/// to differentiate between u32 and fixed32).
/// If modifying this, also look at createFieldEncoding!
const FieldEncoding = union(enum) {
    default, // bool; float/double (f32/f64); submessage (struct with pb_desc)
    fixed, // [s]fixed[32/64]
    varint, // [u]int[32/64]
    zigzag, // sint[32/64]
    string, // string
    repeat: *const FieldEncoding, // repeated (child encoding)
    repeat_pack: *const FieldEncoding, // repeated (child encoding)
    bytes, // bytes
    map: [*]const FieldEncoding, //*const [2]FieldEncoding, // map (k/v encodings)
};

/// A descriptor for a single field, giving its field number and encoding. These should be stored in
/// a 'pb_desc' decl on the message struct. 'oneof' values, represented as optional tagged unions,
/// are the only field type which should not have a corresponding descriptor index, but they must
/// contain their own 'pb_desc' decl describing the fields within them.
const FieldDescriptor = struct {
    field_num: u29,
    encoding: FieldEncoding,
};

/// A Protobuf wire type - all data is encoded as one of these.
const WireType = enum(u3) {
    varint,
    i64,
    len,
    sgroup, // DEPRECATED
    egroup, // DEPRECATED
    i32,
};

/// Encode 'val' into the given writer as LEB128.
fn encodeVarInt(w: anytype, val: u64) !void {
    if (val == 0) {
        try w.writeByte(0);
        return;
    }

    var x = val;
    while (x != 0) {
        const part: u8 = @truncate(x & 0x7f);
        x >>= 7;
        const next: u8 = @intFromBool(x != 0);
        try w.writeByte(next << 7 | part);
    }
}

/// Encode a field tag, composed of a field number and associated wire type.
fn encodeTag(w: anytype, field_num: u29, wire_type: WireType) !void {
    const wire = @intFromEnum(wire_type);
    const val = @as(u32, wire) | @as(u32, field_num) << 3;
    return encodeVarInt(w, val);
}

/// Encode 'val' of scalar type (integer, float, bool, string, or bytes) with field descriptor
/// 'desc' into the given writer. If 'encode_default' is false, the field will be omitted if it
/// corresponds to its type's default value. If 'include_tag' is false, the field's tag is not
/// included in the output.
fn encodeSingleScalar(w: anytype, val: anytype, comptime desc: FieldDescriptor, comptime encode_default: bool, comptime override_default: ?@TypeOf(val), comptime include_tag: bool) !void {
    const T = @TypeOf(val);

    if (@typeInfo(T) == .@"enum") {
        if (desc.encoding != .default) @compileError("Enum types must use FieldEncoding.default");
        const Tag = @typeInfo(T).@"enum".tag_type;
        if (@bitSizeOf(Tag) > 32) @compileError("Enum types must have a tag type of no more than 32 bits");
        const Tag32 = if (@typeInfo(Tag).int.signedness == .signed) i32 else u32;
        const ival: Tag32 = @intFromEnum(val);
        return encodeSingleScalar(
            w,
            ival,
            .{ .field_num = desc.field_num, .encoding = .varint },
            encode_default,
            if (override_default) |x| @intFromEnum(x) else null,
            include_tag,
        );
    }

    switch (T) {
        bool => {
            if (desc.encoding != .default) @compileError("Boolean types must use FieldEncoding.default");
            if (!encode_default and val == (override_default orelse false)) return;
            if (include_tag) try encodeTag(w, desc.field_num, .varint);
            try w.writeByte(@intFromBool(val));
        },

        u32, u64, i32, i64 => {
            if (!encode_default and val == (override_default orelse 0)) return;
            switch (desc.encoding) {
                .fixed => {
                    if (include_tag) try encodeTag(w, desc.field_num, switch (T) {
                        u32, i32 => .i32,
                        u64, i64 => .i64,
                        else => unreachable,
                    });
                    try w.writeInt(T, val, .little);
                },
                .varint => {
                    if (include_tag) try encodeTag(w, desc.field_num, .varint);
                    const val64: u64 = switch (T) {
                        u32, u64 => val,
                        i32 => @bitCast(@as(i64, val)), // sign-extend
                        i64 => @bitCast(val),
                        else => unreachable,
                    };
                    try encodeVarInt(w, val64);
                },
                .zigzag => {
                    if (@typeInfo(T).int.signedness != .signed) @compileError("Only signed integral types can use FieldEncoding.zigzag");
                    if (include_tag) try encodeTag(w, desc.field_num, .varint);
                    if (val >= 0) {
                        try encodeVarInt(w, @as(u64, @intCast(val)) * 2);
                    } else {
                        try encodeVarInt(w, @as(u64, @intCast(-val - 1)) * 2 + 1);
                    }
                },
                else => @compileError("Integral types must use FieldEncoding.fixed, FieldEncoding.varint, or FieldEncoding.zigzag"),
            }
            return;
        },

        f32, f64 => {
            if (desc.encoding != .default) @compileError("Floating types must use FieldEncoding.default");
            if (!encode_default and val == (override_default orelse 0)) return;
            if (T == f32) {
                if (include_tag) try encodeTag(w, desc.field_num, .i32);
                try w.writeInt(u32, @as(u32, @bitCast(val)), .little);
            } else {
                if (include_tag) try encodeTag(w, desc.field_num, .i64);
                try w.writeInt(u64, @as(u64, @bitCast(val)), .little);
            }
        },

        []u8, []const u8 => {
            if (override_default != null) @compileError("Cannot override default for []u8");
            if (!encode_default and val.len == 0) return;
            switch (desc.encoding) {
                .string, .bytes => {
                    if (include_tag) try encodeTag(w, desc.field_num, .len);
                    try encodeVarInt(w, val.len);
                    try w.writeAll(val);
                },
                else => @compileError("[]u8 must use FieldEncoding.string or FieldEncoding.bytes"),
            }
        },

        else => @compileError("Type '" ++ @typeName(T) ++ "' cannot be encoded as a primitive"),
    }
}

/// Encode a single value of scalar or submessage type. 'map's are not included here since
/// they're sugar for a 'repeated' submessage (and cannot themselves be repeated), meaning they are
/// really multiple values.
fn encodeSingleValue(w: anytype, allocator: std.mem.Allocator, val: anytype, comptime desc: FieldDescriptor, comptime encode_default: bool, comptime override_default: ?@TypeOf(val)) !void {
    const T = @TypeOf(val);

    if (@typeInfo(T) == .@"struct") {
        if (desc.encoding != .default) @compileError("Sub-messages must use FieldEncoding.default");

        var buf = std.ArrayList(u8).init(allocator);
        defer buf.deinit();

        try encode(buf.writer(), allocator, val);

        try encodeTag(w, desc.field_num, .len);
        try encodeVarInt(w, buf.items.len);
        try w.writeAll(buf.items);
    } else {
        try encodeSingleScalar(w, val, desc, encode_default, override_default, true);
    }
}

/// Encode the field 'val' with 'desc_opt' as its descriptor (null if none exists) into the given
/// writer. 'field_name' is used only for error messages.
fn encodeAnyField(
    w: anytype,
    allocator: std.mem.Allocator,
    val: anytype,
    comptime desc_opt: ?FieldDescriptor,
    comptime field_name: []const u8,
    comptime field_default: ?@TypeOf(val),
) !void {
    const T = @TypeOf(val);

    // Nicer error message if you forgot to make your union optional
    if (@typeInfo(T) == .@"union") {
        @compileError("Only optional unions can be encoded");
    }

    if (@typeInfo(T) == .optional and
        @typeInfo(std.meta.Child(T)) == .@"union")
    {
        // oneof
        const U = std.meta.Child(T);
        if (desc_opt != null) @compileError("Union '" ++ field_name ++ "' must not have a field descriptor");
        if (val) |un| {
            const pb_desc = comptime getPbDesc(U) orelse @compileError("Union '" ++ @typeName(U) ++ "' must have a pb_desc decl");
            switch (un) {
                inline else => |payload, tag| {
                    const sub_desc = comptime pb_desc.getField(@tagName(tag)) orelse
                        @compileError("Mising descriptor for field '" ++ @typeName(U) ++ "." ++ @tagName(tag) ++ "'");

                    try encodeSingleValue(w, allocator, payload, sub_desc, true, null);
                },
            }
        }

        return;
    }

    const desc = desc_opt orelse @compileError("Missing descriptor for field '" ++ field_name ++ "'");

    if (desc.encoding == .repeat) {
        for (val.items) |x| {
            try encodeSingleValue(w, allocator, x, .{
                .field_num = desc.field_num,
                .encoding = desc.encoding.repeat.*,
            }, true, null);
        }
    } else if (desc.encoding == .repeat_pack) {
        var buf = std.ArrayList(u8).init(allocator);
        defer buf.deinit();

        for (val.items) |x| {
            try encodeSingleScalar(buf.writer(), x, .{
                .field_num = desc.field_num,
                .encoding = desc.encoding.repeat_pack.*,
            }, true, null, false);
        }

        try encodeTag(w, desc.field_num, .len);
        try encodeVarInt(w, buf.items.len);
        try w.writeAll(buf.items);
    } else if (desc.encoding == .map) {
        var it = val.iterator();
        while (it.next()) |pair| {
            try encodeSingleValue(w, allocator, struct {
                k: std.meta.FieldType(T.KV, .key),
                v: std.meta.FieldType(T.KV, .value),
                const pb_desc = .{
                    .k = .{ 1, desc.encoding.map[0] },
                    .v = .{ 2, desc.encoding.map[1] },
                };
            }{ .k = pair.key_ptr.*, .v = pair.value_ptr.* }, .{
                .field_num = desc.field_num,
                .encoding = .default,
            }, true, null);
        }
    } else if (@typeInfo(T) == .optional) {
        if (val) |x| {
            try encodeSingleValue(w, allocator, x, desc, true, null);
        }
    } else {
        try encodeSingleValue(w, allocator, val, desc, false, field_default);
    }
}

/// Encode an entire Protobuf message 'msg' into the given writer. Only temporary allocations are
/// performed, all of which are cleaned up before this function returns.
pub fn encode(allocator: std.mem.Allocator, writer: anytype, msg: anytype) !void {
    const Msg = @TypeOf(msg);
    const pb_desc = comptime getPbDesc(Msg) orelse
        @compileError("Message type '" ++ @typeName(Msg) ++ "' must have a pb_desc decl");

    validateDescriptors(Msg);

    inline for (@typeInfo(Msg).@"struct".fields) |field| {
        const desc: ?FieldDescriptor = comptime pb_desc.getField(field.name);

        const default: ?field.type = if (field.default_value) |ptr|
            @as(*const field.type, @ptrCast(ptr)).*
        else
            null;

        try encodeAnyField(writer, allocator, @field(msg, field.name), desc, @typeName(Msg) ++ "." ++ field.name, default);
    }
}

/// Perform some basic checks on the field descriptors in the message type 'Msg', ensuring every
/// descriptor corresponds to a field and that field numbers appear at most once.
fn validateDescriptors(comptime Msg: type) void {
    comptime {
        var seen_field_nums: []const u29 = &.{};
        validateDescriptorsInner(Msg, &seen_field_nums);
        for (seen_field_nums, 0..) |x, i| {
            for (seen_field_nums[i + 1 ..]) |y| {
                if (x == y) {
                    @compileError(std.fmt.comptimePrint("Duplicate field number {} in type '{s}'", .{ x, @typeName(Msg) }));
                }
            }
        }
    }
}

fn validateDescriptorsInner(comptime Msg: type, comptime seen_field_nums: *[]const u29) void {
    const pb_desc = comptime getPbDesc(Msg).?;
    for (pb_desc.fields) |field_desc| {
        const name = field_desc[0];
        if (!@hasField(Msg, name)) {
            @compileError("Descriptor '" ++ name ++ "' does not correspond to any field in type '" + @typeName(Msg));
        }
        seen_field_nums.* = seen_field_nums.* ++ &[1]u29{field_desc[1].field_num};
    }

    for (std.meta.fields(Msg)) |field| {
        if (@typeInfo(field.type) == .@"struct" and comptime getPbDesc(field.type) != null) {
            validateDescriptors(field.type);
        } else if (@typeInfo(field.type) == .optional and
            @typeInfo(std.meta.Child(field.type)) == .@"union" and
            comptime getPbDesc(std.meta.Child(field.type)) != null)
        {
            validateDescriptorsInner(std.meta.Child(field.type), seen_field_nums);
        }
    }
}

fn initDefault(comptime Msg: type, allocator: std.mem.Allocator) Msg {
    var result: Msg = undefined;

    inline for (comptime std.meta.fields(Msg)) |field| {
        switch (@typeInfo(field.type)) {
            .pointer => |info| if (info.size == .Slice) {
                @field(result, field.name) = &.{};
                continue;
            },
            else => {},
        }

        const default: ?field.type = if (field.default_value) |ptr|
            @as(*const field.type, @ptrCast(ptr)).*
        else
            null;

        @field(result, field.name) = switch (@typeInfo(field.type)) {
            .optional => default orelse null,
            .int, .float => default orelse 0,
            .@"enum" => |e| default orelse if (e.is_exhaustive)
                comptime std.meta.intToEnum(field.type, 0) catch
                    @compileError("Enum '" ++ @typeName(field.type) ++ "' has no 0 default")
            else
                @enumFromInt(0),
            .bool => default orelse false,
            .@"struct" => if (comptime getPbDesc(field.type) != null)
                initDefault(field.type, allocator)
            else
                field.type{},
            else => @compileError("Type '" ++ @typeName(field.type) ++ "' cannot be deserialized"),
        };
    }

    return result;
}

fn decodeVarInt(r: anytype) !u64 {
    var shift: u6 = 0;
    var x: u64 = 0;
    while (true) {
        const b = try r.readByte();
        x |= @as(u64, b & 0x7f) << shift;
        if (b >> 7 == 0) break;
        shift += 7;
    }
    return x;
}

fn skipField(r: anytype, wire_type: WireType, field_num: u29) !void {
    switch (wire_type) {
        .varint => _ = try decodeVarInt(r),
        .i64 => _ = try r.readInt(u64, .little),
        .len => {
            const len = try decodeVarInt(r);
            try r.skipBytes(len, .{});
        },
        .sgroup => {
            while (true) {
                const tag = try decodeVarInt(r);
                const sub_wire = std.meta.intToEnum(WireType, tag & 7) catch return error.MalformedInput;
                const sub_num = std.math.cast(u29, tag >> 3) orelse return error.MalformedInput;
                if (sub_wire == .egroup and sub_num == field_num) {
                    break;
                }
                try skipField(r, sub_wire, sub_num);
            }
        },
        .egroup => return error.MalformedInput,
        .i32 => _ = try r.readInt(u32, .little),
    }
}

fn decodeSingleScalar(comptime T: type, comptime encoding: FieldEncoding, r: anytype, allocator: std.mem.Allocator, wire_type: WireType) !T {
    if (@typeInfo(T) == .@"enum") {
        if (encoding != .default) @compileError("Enum types must use FieldEncoding.default");
        const Tag = @typeInfo(T).@"enum".tag_type;
        if (@bitSizeOf(Tag) > 32) @compileError("Enum types must have a tag type of no more than 32 bits");
        const Tag32 = if (@typeInfo(Tag).int.signedness == .signed) i32 else u32;
        const ival = try decodeSingleScalar(Tag32, .varint, r, allocator, wire_type);
        return try std.meta.intToEnum(T, ival);
    }

    switch (T) {
        bool => {
            if (encoding != .default) @compileError("Boolean types must use FieldEncoding.default");
            if (wire_type != .varint) return error.MalformedInput;
            const x = try decodeVarInt(r);
            return @as(u32, @truncate(x)) != 0;
        },

        u32, u64, i32, i64 => {
            switch (encoding) {
                .fixed => {
                    switch (T) {
                        u32, i32 => if (wire_type != .i32) return error.MalformedInput,
                        u64, i64 => if (wire_type != .i64) return error.MalformedInput,
                        else => unreachable,
                    }
                    return r.readInt(T, .little);
                },
                .varint => {
                    if (wire_type != .varint) return error.MalformedInput;
                    const Unsigned = switch (T) {
                        u32, i32 => u32,
                        u64, i64 => u64,
                        else => unreachable,
                    };
                    return @bitCast(@as(Unsigned, @truncate(try decodeVarInt(r))));
                },
                .zigzag => {
                    if (@typeInfo(T).int.signedness != .signed) @compileError("Only signed integral types can use FieldEncoding.zigzag");
                    if (wire_type != .varint) return error.MalformedInput;
                    const raw = try decodeVarInt(r);
                    const val = if (raw % 2 == 1)
                        -@as(i64, @intCast(raw / 2)) - 1
                    else
                        @as(i64, @intCast(raw / 2));
                    return @truncate(val);
                },
                else => @compileError("Integral types must use FieldEncoding.fixed, FieldEncoding.varint, or FieldEncoding.zigzag"),
            }
        },

        f32, f64 => {
            if (encoding != .default) @compileError("Floating types must use FieldEncoding.default");
            if (T == f32) {
                if (wire_type != .i32) return error.MalformedInput;
                return @bitCast(try r.readInt(u32, .little));
            } else {
                if (wire_type != .i64) return error.MalformedInput;
                return @bitCast(try r.readInt(u64, .little));
            }
        },

        []u8, []const u8 => {
            if (encoding != .string and encoding != .bytes) @compileError("[]u8 must use FieldEncoding.string or FieldEncoding.bytes");
            if (wire_type != .len) return error.MalformedInput;
            const len = try decodeVarInt(r);
            const buf = try allocator.alloc(u8, len);
            try r.readNoEof(buf);
            return buf;
        },

        else => @compileError("Type '" ++ @typeName(T) ++ "' cannot be decoded as a primitive"),
    }
}

/// Decodes a value of scalar or submessage type, returning the result.
fn decodeSingleValue(comptime T: type, comptime encoding: FieldEncoding, r: anytype, allocator: std.mem.Allocator, wire_type: WireType) !T {
    if (@typeInfo(T) == .@"struct") {
        if (encoding != .default) @compileError("Sub-messages must use FieldEncoding.default");
        if (wire_type != .len) return error.MalformedInput;
        const len = try decodeVarInt(r);
        var lr = std.io.limitedReader(r, len);
        return decode(T, lr.reader(), allocator);
    } else {
        return decodeSingleScalar(T, encoding, r, allocator, wire_type);
    }
}

/// Attempts to decode a field of any type, modifying the result location as necessary (either
/// overwriting the value or appending data). Returns true if this message corresponded to the given
/// field (and was decoded).
fn maybeDecodeAnyField(comptime T: type, comptime desc_opt: ?FieldDescriptor, comptime field_name: []const u8, r: anytype, allocator: std.mem.Allocator, wire_type: WireType, field_num: u29, result: *T) !bool {
    // Nicer error message if you forgot to make your union optional
    if (@typeInfo(T) == .@"union") {
        @compileError("Only optional unions can be decoded");
    }

    if (@typeInfo(T) == .optional and @typeInfo(std.meta.Child(T)) == .@"union") {
        if (desc_opt != null) @compileError("Union must not have a field descriptor");
        if (try maybeDecodeOneOf(std.meta.Child(T), r, allocator, wire_type, field_num)) |val| {
            result.* = val;
            return true;
        } else {
            return false;
        }
    }

    const desc = desc_opt orelse @compileError("Missing descriptor for field '" ++ field_name ++ "'");

    if (field_num != desc.field_num) return false;

    if (desc.encoding == .repeat or desc.encoding == .repeat_pack) {
        const Elem = std.meta.Child(T.Slice);
        const scalar_elem = switch (@typeInfo(Elem)) {
            .int, .bool, .float => true,
            else => Elem == []u8 or Elem == []const u8,
        };
        if (desc.encoding == .repeat_pack and !scalar_elem) {
            @compileError("Packed repeated fields must be slices of scalar types");
        }
        const child_enc = switch (desc.encoding) {
            .repeat, .repeat_pack => |e| e.*,
            else => unreachable,
        };
        // By spec, decoders should be able to decode non-packed repeated fields as packed and vice
        // versa, so that the protocol can be changed whilst preserving forwards and backwards
        // compatibility.
        if (scalar_elem) {
            if (wire_type == .len) {
                const len = try decodeVarInt(r);
                var lr = std.io.limitedReader(r, len);
                const expect_wire: WireType = switch (child_enc) {
                    .fixed => switch (Elem) {
                        u32, i32, f32 => .i32,
                        u64, i64, f64 => .i64,
                        else => undefined, // not unreachable to defer to nice error handling in decodeSingleScalar
                    },
                    .varint, .zigzag => .varint,
                    .string, .bytes => .len,
                    .default => switch (Elem) {
                        bool => .varint,
                        f32 => .i32,
                        f64 => .i64,
                        else => undefined, // not unreachable to defer to nice error handling in decodeSingleScalar
                    },
                    else => undefined,
                };

                while (decodeSingleScalar(Elem, child_enc, lr.reader(), allocator, expect_wire)) |elem| {
                    try result.*.append(allocator, elem);
                } else |err| switch (err) {
                    error.EndOfStream => {},
                    else => |e| return e,
                }

                return true;
            }
        }

        const elem = try decodeSingleValue(Elem, child_enc, r, allocator, wire_type);
        try result.*.append(allocator, elem);
    } else if (desc.encoding == .map) {
        const val = try decodeSingleValue(struct {
            k: std.meta.FieldType(T.KV, .key),
            v: std.meta.FieldType(T.KV, .value),
            const pb_desc = .{
                .k = .{ 1, desc.encoding.map[0] },
                .v = .{ 2, desc.encoding.map[1] },
            };
        }, .default, r, allocator, wire_type);
        try result.put(allocator, val.k, val.v);
    } else if (@typeInfo(T) == .optional) {
        result.* = try decodeSingleValue(std.meta.Child(T), desc.encoding, r, allocator, wire_type);
    } else {
        result.* = try decodeSingleValue(T, desc.encoding, r, allocator, wire_type);
    }

    return true;
}

fn maybeDecodeOneOf(comptime U: type, r: anytype, allocator: std.mem.Allocator, wire_type: WireType, field_num: u29) !?U {
    const pb_desc = comptime getPbDesc(U) orelse
        @compileError("Union '" ++ @typeName(U) ++ "' must have a pb_desc decl");

    inline for (std.meta.fields(U)) |field| {
        const desc = comptime pb_desc.getField(field.name) orelse
            @compileError("Missing descriptor for field '" ++ @typeName(U) ++ "." ++ field.name ++ "'");

        if (desc.field_num == field_num) {
            const payload = try decodeSingleValue(field.type, desc.encoding, r, allocator, wire_type);
            return @unionInit(U, field.name, payload);
        }
    }

    return null;
}

pub fn decode(comptime Msg: type, allocator: std.mem.Allocator, reader: anytype) !Msg {
    const pb_desc = comptime getPbDesc(Msg) orelse @compileError("Message type '" ++ @typeName(Msg) ++ "' must have a pb_desc decl");
    validateDescriptors(Msg);

    var result = initDefault(Msg, allocator);

    while (decodeVarInt(reader)) |tag| {
        const wire_type = std.meta.intToEnum(WireType, tag & 7) catch return error.MalformedInput;
        const field_num = std.math.cast(u29, tag >> 3) orelse return error.MalformedInput;

        inline for (std.meta.fields(Msg)) |field| {
            const desc_opt: ?FieldDescriptor = comptime pb_desc.getField(field.name);

            if (try maybeDecodeAnyField(field.type, desc_opt, @typeName(Msg) ++ "." ++ field.name, reader, allocator, wire_type, field_num, &@field(result, field.name))) {
                break;
            }
        } else {
            try skipField(reader, wire_type, field_num);
        }
    } else |err| switch (err) {
        error.EndOfStream => {},
        else => |e| return e,
    }

    return result;
}

const PbDesc = struct {
    const Entry = struct { []const u8, FieldDescriptor };
    fields: []const Entry,

    fn getField(self: PbDesc, name: []const u8) ?FieldDescriptor {
        for (self.fields) |f| {
            if (std.mem.eql(u8, f[0], name)) return f[1];
        }
        return null;
    }
};

// Directly making a pb_desc with fields of type FieldDescriptor is quite inconvenient, so instead
// we'll take big literals in the same shape and parse them into the real descriptors.

fn getPbDesc(comptime T: type) ?PbDesc {
    comptime {
        if (!@hasDecl(T, "pb_desc")) return null;
        const desc = T.pb_desc;

        var fields: []const PbDesc.Entry = &.{};

        for (std.meta.fields(@TypeOf(desc))) |field| {
            const fd = createFieldDesc(@field(desc, field.name), @typeName(T) ++ "." ++ field.name);
            fields = fields ++ &[1]PbDesc.Entry{.{ field.name, fd }};
        }

        return .{ .fields = fields };
    }
}

fn createFieldDesc(comptime desc: anytype, comptime field_name: []const u8) FieldDescriptor {
    switch (@typeInfo(@TypeOf(desc))) {
        .@"struct" => |info| if (info.is_tuple) return .{
            .field_num = desc[0],
            .encoding = createFieldEncoding(desc[1], field_name),
        },
        else => {},
    }

    @compileError("Bad descriptor format for field '" ++ field_name ++ "'");
}

fn createFieldEncoding(comptime enc: anytype, comptime field_name: []const u8) FieldEncoding {
    if (@TypeOf(enc) == FieldEncoding) {
        return enc;
    } else if (@TypeOf(enc) == @Type(.enum_literal)) {
        // try to match with an encoding type
        for (std.meta.fields(FieldEncoding)) |field| {
            if (std.mem.eql(u8, @tagName(enc), field.name)) {
                return @field(FieldEncoding, field.name);
            }
        }
    } else if (@typeInfo(@TypeOf(enc)) == .@"struct") {
        // nested encoding types
        const fields = @typeInfo(@TypeOf(enc)).@"struct".fields;
        if (fields.len == 1) {
            const tag = fields[0].name;
            const val = @field(enc, tag);
            if (std.mem.eql(u8, tag, "repeat")) {
                const child = createFieldEncoding(val, field_name);
                return .{ .repeat = &child };
            } else if (std.mem.eql(u8, tag, "repeat_pack")) {
                const child = createFieldEncoding(val, field_name);
                return .{ .repeat_pack = &child };
            } else if (std.mem.eql(u8, tag, "map")) {
                switch (@typeInfo(@TypeOf(val))) {
                    .@"struct" => |info| if (info.is_tuple and val.len == 2) {
                        const child0 = createFieldEncoding(val[0], field_name);
                        const child1 = createFieldEncoding(val[1], field_name);
                        return .{ .map = &[2]FieldEncoding{ child0, child1 } };
                    },
                    else => {},
                }
            }
        }
    }

    @compileError("Bad encoding for field '" ++ field_name ++ "'");
}
