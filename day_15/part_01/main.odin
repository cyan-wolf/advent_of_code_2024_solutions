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

    // Represents displacement.
    delta := Pos {}

    switch move {
        case '>':
            delta.col = 1
            
        case '<':
            delta.col = -1

        case '^':
            delta.row = -1

        case 'v':
            delta.row = 1
    }

    canMove := false

    ch: rune
    for {
        ch = getAt(grid, pos)

        if ch == '!' || ch == '#' || ch == '.' {
            // This means that there's space to move.
            if ch == '.' {
                canMove = true
            }
            break
        }

        pos.row += delta.row
        pos.col += delta.col

        append(&acc, ch)
    }

    // Update the grid if a move can be performed.
    if canMove {
        performMove(robotPos^, delta, acc, grid)

        // Update the robot's position.
        robotPos.row += delta.row
        robotPos.col += delta.col
    }
}

// Mutates the grid after a move has been processed.
performMove :: proc(startPos: Pos, delta: Pos, acc: [dynamic]rune, grid: ^Grid) {
    // Copy the start position into a new variable,
    // since parameters are readonly.
    pos := startPos

    // Leave an empty space where the player was.
    grid[pos.row][pos.col] = '.'

    for ch in acc {
        // Update the position.
        pos.row += delta.row
        pos.col += delta.col

        // Mutate the grid itself.
        grid[pos.row][pos.col] = ch
    }
}

showGrid :: proc(grid: Grid) {
    for row in grid {
        fmt.println(row)
    }
}

// Calculates the GPS score according to the 
// problem statement.
calcGpsScore :: proc(grid: Grid) -> int {
    score := 0

    for r := 0; r < len(grid); r += 1 {
        for c := 0; c < len(grid[0]); c+= 1 {
            if grid[r][c] == 'O' {
                score += 100 * r + c
            }
        }
    }
    return score
}

main :: proc() {
    filepath := os.args[1]

    grid, moves, robotPos := readInput(filepath)
    defer delete(grid)
    defer delete(moves)

    for move in moves {
        processMove(move, &grid, &robotPos)
    }
    fmt.println(calcGpsScore(grid))
}