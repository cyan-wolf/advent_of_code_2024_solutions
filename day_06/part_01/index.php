<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Advent of Code 2024</title>
    <style>
        body {
            text-align: center;
            color: lime;
            background-color: black;
        }
    </style>
</head>
<body>
    <?php 
        // Reads the input as a 2D array and finds the guard's position.
        function read_input() {
            $guard_pos = [-1, -1];
            $row = 0;

            $file = fopen("input.txt", "r");
            $lines = array();

            if ($file) {
                while (($line = fgets($file)) !== false) {
                    $line_arr = str_split(trim($line));

                    for ($col = 0; $col < count($line_arr); $col++) {
                        if ($line_arr[$col] == '^') {
                            $guard_pos = [$row, $col];
                            break;
                        }
                    }

                    $lines[] = $line_arr;
                    $row++;
                }
                fclose($file);
            }
            return [$lines, $guard_pos];
        }

        // Rotates the given direction by 90 degrees (clock-wise).
        function rotate($dir) {
            switch ($dir) {
                case "N":
                    return "E";

                case "E":
                    return "S";

                case "S":
                    return "W";

                case "W":
                    return "N";

                default:
                    die("error: invalid direction");
            }
        }

        // Checks whether the given guard position is in-bounds.
        function in_bounds($pos, $rows, $cols) {
            [$r, $c] = $pos;

            return !($r < 0 || $r >= $rows || $c < 0 || $c >= $cols);
        }

        // Returns the forward position and the character at tha position.
        function peek_forward($pos, $dir, $map) {
            $forward_pos = [-1, -1];

            switch ($dir) {
                case "N":
                    $forward_pos = [$pos[0] - 1, $pos[1]];
                    break;

                case "E":
                    $forward_pos = [$pos[0], $pos[1] + 1];
                    break;

                case "S":
                    $forward_pos = [$pos[0] + 1, $pos[1]];
                    break;

                case "W":
                    $forward_pos = [$pos[0], $pos[1] - 1];
                    break;

                default:
                    die("error: invalid direction");
            }

            if (in_bounds($forward_pos, count($map), count($map[0]))) {
                $next_char = $map[$forward_pos[0]][$forward_pos[1]];
                return [$next_char, $forward_pos];
            }
            return ["X", $forward_pos]; // "X" is a placeholder
        }

        // Returns a unique representation of a position.
        // (This is used to save the position in the hash set).
        function pos_to_str($pos) {
            return $pos[0] . " " . $pos[1];
        }

        // Returns the number of distinct visited positions by the guard 
        // after following the protocol in the problem statement.
        function get_distinct_visited_positions() {
            [$map, $guard_pos] = read_input();
            $dir = "N";

            $rows = count($map);
            $cols = count($map[0]);

            $visited_positions = array(); // hash set

            while (in_bounds($guard_pos, $rows, $cols)) {
                // Mark the current position as visited. 
                // (The value is null since this is a hash set).
                $visited_positions[pos_to_str($guard_pos)] = null;

                [$next_char, $next_pos] = peek_forward($guard_pos, $dir, $map);

                // Rotate since there is an obstacle.
                if ($next_char == "#") {
                    $dir = rotate($dir);
                }
                // Otherwise, move the guard forward, even if
                // they end up out of bounds.
                else {
                    $guard_pos = $next_pos;
                }
            }

            // The size of the hash set is the number of 
            // distinct visited positions.
            return count($visited_positions);
        }
    ?>

    <h1>Advent of Code 2024 - Day 6</h1>

    <p>Result: <?php echo get_distinct_visited_positions(); ?></p>
</body>
</html>