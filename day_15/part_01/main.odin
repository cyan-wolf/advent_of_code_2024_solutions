package main

import "core:fmt"
import "core:os"
import "core:strings"

Pos :: struct {
    row: int,
    col: int,
}

Grid :: [dynamic][dynamic]rune

// Reads the input file as a grid, a series of moves, and the robot's position.
readInput :: proc(filepath: string) -> (
    grid: Grid,
    moves: [dynamic]rune, 
    robotPos: Pos,
) {
    data, ok := os.read_entire_file(filepath, context.allocator)
    if !ok {
        fmt.println("error: could not read file")
        return;
    }
    defer delete(data, context.allocator)

    reading_grid := true

    it := string(data)
    for line in strings.split_lines_iterator(&it) {
        if line == "" {
            reading_grid = false
        }
        row := 0

        if reading_grid {
            chars: [dynamic]rune

            for char, col in line {
                if char == '@' {
                    robotPos = {
                        row, col
                    }
                }
                
                append(&chars, char)
                row += 1
            }
            append(&grid, chars)
        }
        else {
            for c in line {
                append(&moves, c)
            }
        }
    }
    return grid, moves, robotPos
}

// Gets a rune from the grid at the given position.
// Returns the rune ```'!'``` if out of bounds.
getAt :: proc(grid: ^Grid, pos: Pos) -> rune {
    rows := len(grid)
    cols := len(grid[0])

    if pos.row >= 0 && pos.row < rows && pos.col >= 0 && pos.col < cols {
        return grid[pos.row][pos.col]
    }
    return '!' // out of bounds
}

// Updates the grid according to the physics described in the problem statement.
processMove :: proc(move: rune, grid: ^Grid, robotPos: ^Pos) {
    pos := robotPos^ // copy the robot's current position

    acc: [dynamic]rune
    defer delete(acc)

    switch move {
        case '>':
            // TODO
            //fmt.println("RIGHT")

            ch := getAt(grid, pos)

            for ch != '!' {
                pos.row += 1
                
                // TODO
                append(&acc, ch)

                ch = getAt(grid, pos)
            }

        case '<':
            // TODO
            fmt.println("LEFT")

        case '^':
            // TODO
            fmt.println("UP")

        case 'v':
            // TODO
            fmt.println("DOWN")
    }
}

main :: proc() {
    filepath := os.args[1]

    grid, moves, robotPos := readInput(filepath)
    defer delete(grid)
    defer delete(moves)

    for move in moves {
        processMove(move, &grid, &robotPos)
    }
}