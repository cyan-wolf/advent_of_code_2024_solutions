const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Pair = struct {
    x: i32,
    y: i32,
};

const Robot = struct {
    pos: Pair,
    vel: Pair,
};

fn parseVector(vecStr: []const u8) !Pair {
    var parts = std.mem.split(u8, vecStr, "=");

    // Skip the vector label.
    _ = parts.next();

    var components = std.mem.split(u8, parts.next() orelse unreachable, ",");

    const x = try std.fmt.parseInt(i32, components.next() orelse unreachable, 10);
    const y = try std.fmt.parseInt(i32, components.next() orelse unreachable, 10);

    return .{ .x = x, .y = y };
}

fn processLine(line: []const u8) !Robot {
    var parts = std.mem.split(u8, line, " ");

    const posPart = try parseVector(parts.next() orelse unreachable);
    const velPart = try parseVector(parts.next() orelse unreachable);

    return .{ .pos = posPart, .vel = velPart };
}

fn fillRobotsList(list: *ArrayList(Robot), fileName: []const u8) !void {
    var file = try std.fs.cwd().openFile(fileName, .{});
    defer file.close();

    var bufReader = std.io.bufferedReader(file.reader());
    var inStream = bufReader.reader();

    var buf: [1024]u8 = undefined;
    while (try inStream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const trimmedLine = std.mem.trimRight(u8, line, "\r");

        const robot = try processLine(trimmedLine);
        try list.append(robot);
    }
}

fn updateRobots(robots: *ArrayList(Robot), spaceDimensions: Pair) void {
    for (0..robots.items.len) |i| {
        const pos = robots.items[i].pos;
        const vel = robots.items[i].vel;

        robots.items[i].pos.x = @mod(pos.x + vel.x, spaceDimensions.x);
        robots.items[i].pos.y = @mod(pos.y + vel.y, spaceDimensions.y);
    }
}

// Used for waiting for user input.
fn pauseForInput() !void {
    const stdin = std.io.getStdIn().reader();
    var buffer: [8]u8 = undefined;
    _ = try stdin.readUntilDelimiterOrEof(buffer[0..], '\n');
}

pub fn main() !void {
    // Setup allocator.
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    // Read command-line arguments to get the input file's name.
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip(); // skip program name
    const fileName = args.next() orelse "input_test.txt";

    // Read robots from input file.
    var robotsList = ArrayList(Robot).init(allocator);
    defer robotsList.deinit();

    try fillRobotsList(&robotsList, fileName);

    // Simulate and display the robots in the space.
    const spaceDimensions = Pair{ .x = 101, .y = 103 };

    var space = [_][spaceDimensions.x]u8{[_]u8{'.'} ** spaceDimensions.x} ** spaceDimensions.y;

    for (1..1000) |elapsed| {
        updateRobots(&robotsList, spaceDimensions);

        for (robotsList.items) |robot| {
            const x: usize = @intCast(robot.pos.x);
            const y: usize = @intCast(robot.pos.y);

            space[y][x] = '#';
        }

        std.debug.print("\x1B[2J\x1B[H", .{}); // clear screen

        for (0..spaceDimensions.y) |y| {
            const row = space[y];

            std.debug.print("{s}\n", .{row}); // display row

            space[y] = [_]u8{'.'} ** spaceDimensions.x; // clear row
        }

        std.debug.print("Elapsed: {d}\n", .{elapsed});
        try pauseForInput();
    }
}
