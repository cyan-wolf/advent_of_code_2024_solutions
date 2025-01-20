// Incorrect solution.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const print = std.debug.print;

const Ordering = enum { none, less, greater };

// A report only counts as safe if both of the following are true:
// - The levels are either all increasing or all decreasing.
// - Any two adjacent levels differ by at least one and at most three.

// Check a pair of numbers. Determine if there is a problem and the ordering.
fn check_pair(a: i32, b: i32, dir: Ordering) struct { found_problem: bool, dir: Ordering } {
    const step = @abs(b - a);
    var found_problem = false;
    var new_dir = dir;

    if (a < b) {
        if (dir == Ordering.greater) {
            found_problem = true;
        }
        new_dir = Ordering.less;
    } else if (a > b) {
        if (dir == Ordering.less) {
            found_problem = true;
        }
        new_dir = Ordering.greater;
    }

    if ((step < 1) or (step > 3)) {
        found_problem = true;
    }
    return .{ .found_problem = found_problem, .dir = new_dir };
}

// Determine if a line (report) is safe.
fn report_is_safe(line: []const u8, allocator: Allocator) !bool {
    var splits = std.mem.split(u8, line, " ");

    // Collect the line's numbers into an `ArrayList`.
    var nums = ArrayList(i32).init(allocator);
    defer nums.deinit();

    while (splits.next()) |num_str| {
        const num = try std.fmt.parseInt(i32, num_str, 10);
        try nums.append(num);
    }

    // Used to keep track of the current ordering direction.
    var curr_dir = Ordering.none;
    var prev: i32 = undefined;

    var is_valid = true;

    for (0..nums.items.len - 1) |i| {
        const curr = nums.items[i];
        const next = nums.items[i + 1];

        const res = check_pair(curr, next, curr_dir);

        if (res.found_problem) {
            var a = prev;
            var b = next;

            if (i == 0) {
                a = curr;
                b = nums.items[i + 2];
            }

            const res2 = check_pair(a, b, curr_dir);

            if (res2.found_problem) {
                is_valid = false;
                break;
            } else {
                curr_dir = res2.dir;
            }
        } else {
            curr_dir = res.dir;
        }
        prev = curr;
    }

    return is_valid;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var safe_reports: u32 = 0;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const line_trimmed = std.mem.trimRight(u8, line, "\r");

        if (try report_is_safe(line_trimmed, allocator)) {
            safe_reports += 1;
        }
    }

    std.debug.print("{d}\n", .{safe_reports});
}
