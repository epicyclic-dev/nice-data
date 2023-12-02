const std = @import("std");

const nice = @import("nice");

fn reifyScalar(comptime scalar: []const u8, expected: anytype) !void {
    try reifyScalarWithOptions(scalar, expected, .{});
}

fn reifyScalarWithOptions(comptime scalar: []const u8, expected: anytype, options: nice.parser.Options) !void {
    const allocator = std.testing.allocator;
    var diagnostics = nice.Diagnostics{};
    const parsed = try nice.parseBufferTo(
        @TypeOf(expected),
        allocator,
        scalar ++ "\n",
        &diagnostics,
        options,
    );
    defer parsed.deinit();

    try std.testing.expectEqual(expected, parsed.value);
}

test "reify integer" {
    try reifyScalar("123", @as(u8, 123));
    try reifyScalar("0123", @as(u8, 123));
    try reifyScalar("1_23", @as(u8, 123));
    try reifyScalar("-01_23", @as(i8, -123));
}

test "reify hexadecimal" {
    try reifyScalar("0x123", @as(i64, 0x123));
    try reifyScalar("0x0123", @as(i64, 0x123));
    try reifyScalar("0x01_23", @as(i64, 0x123));
    try reifyScalar("-0x01_23", @as(i64, -0x123));
}

test "reify octal" {
    try reifyScalar("0o123", @as(i64, 0o123));
    try reifyScalar("0o0123", @as(i64, 0o123));
    try reifyScalar("0o01_23", @as(i64, 0o123));
    try reifyScalar("-0o01_23", @as(i64, -0o123));
}

test "reify binary" {
    try reifyScalar("0b1011", @as(i5, 0b1011));
    try reifyScalar("0b01011", @as(i5, 0b1011));
    try reifyScalar("0b010_11", @as(i5, 0b1011));
    try reifyScalar("-0b010_11", @as(i5, -0b1011));
}

test "reify float" {
    try reifyScalar("0.25", @as(f32, 0.25));
    try reifyScalar("0.2_5", @as(f32, 0.25));
    try reifyScalar("00.250", @as(f32, 0.25));
    try reifyScalar("-0.25", @as(f32, -0.25));
}

test "reify hexfloat" {
    try reifyScalar("0x0.25", @as(f64, 0x0.25));
    try reifyScalar("0x0.2_5", @as(f64, 0x0.25));
    try reifyScalar("0x0.250p1", @as(f64, 0x0.25p1));
    try reifyScalar("-0x0.25", @as(f64, -0x0.25));
}

test "reify true" {
    try reifyScalar("true", true);
    try reifyScalar("True", true);
    try reifyScalar("yes", true);
    try reifyScalar("on", true);
}

test "reify false" {
    try reifyScalar("false", false);
    try reifyScalar("False", false);
    try reifyScalar("no", false);
    try reifyScalar("off", false);
}

test "reify custom true" {
    const options = nice.parser.Options{ .truthy_boolean_scalars = &.{"correct"} };
    try reifyScalarWithOptions("correct", true, options);
}

test "reify true case insensitive" {
    try std.testing.expectError(error.BadValue, reifyScalar("TRUE", true));
    const options = nice.parser.Options{ .case_insensitive_scalar_coersion = true };
    try reifyScalarWithOptions("TRUE", true, options);
}

test "reify custom false" {
    const options = nice.parser.Options{ .falsy_boolean_scalars = &.{"incorrect"} };
    try reifyScalarWithOptions("incorrect", false, options);
}

test "reify false case insensitive" {
    try std.testing.expectError(error.BadValue, reifyScalar("FALSE", false));
    const options = nice.parser.Options{ .case_insensitive_scalar_coersion = true };
    try reifyScalarWithOptions("FALSE", false, options);
}

test "reify null" {
    try reifyScalar("null", @as(?u8, null));
    try reifyScalar("nil", @as(?u8, null));
    try reifyScalar("None", @as(?u8, null));
}

test "reify custom null" {
    const options = nice.parser.Options{ .null_scalars = &.{"nothing"} };
    try reifyScalarWithOptions("nothing", @as(?u8, null), options);
}

test "reify null case insensitive" {
    // this is a little weird because when the null string mismatches, it will try to
    // parse the child optional type and produce either a value or an error from that,
    // so the error received depends on whether or not the optional child type fails to
    // parse the given value.
    try std.testing.expectError(error.InvalidCharacter, reifyScalar("NULL", @as(?u8, null)));
    const options = nice.parser.Options{ .case_insensitive_scalar_coersion = true };
    try reifyScalarWithOptions("NULL", @as(?u8, null), options);
}

test "reify void" {
    // A void scalar cannot exist on its own as it is not distinguishable from an empty
    // document.
    const Void = struct { void: void };
    try reifyScalar("void:", Void{ .void = void{} });
}

test "reify void scalar" {
    const options = nice.parser.Options{ .default_object = .scalar };
    try reifyScalarWithOptions("", void{}, options);
}

test "reify enum" {
    const Enum = enum { one, two };
    try reifyScalar(".one", Enum.one);
}

test "reify enum no dot" {
    const options = nice.parser.Options{ .expect_enum_dot = false };
    const Enum = enum { one, two };
    try reifyScalarWithOptions("two", Enum.two, options);
}
