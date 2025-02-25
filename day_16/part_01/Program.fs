// INCOMPLETE: Solves the test input files,
// but does not solve the `input.txt` file 
// (algorithm is not efficient enough).

open System

type Pos = (int * int)
type Grid = char array array

// Parsed from the input file; needed for the algorithm.
type Input = {
    Grid: Grid
    StartPos: Pos
    EndPos: Pos
}

// The direction of the agent moving through the maze.
type Direction = 
    | East
    | North
    | West
    | South

// Represents a possible universe of traversed positions.
// Stores the current position and direction of the agent
// along with the accumulated points.
type Universe = {
    SeenPositions: Pos Set
    AccPoints: int
    CurrPos: Pos
    CurrDir: Direction
}

// Parses relevant data from the input file.
let readInput filename = 
    // Representation of the maze.
    let grid = 
        IO.File.ReadAllLines filename
            |> Array.map (fun s -> s.ToCharArray())

    let mutable startPos = -1, -1
    let mutable endPos = -1, -1

    // Find the start and end positions of the maze.
    for r in 0..grid.Length - 1 do
        for c in 0..grid.Length - 1 do
            match grid[r][c] with
            | 'S' -> 
                startPos <- r, c
            | 'E' -> 
                endPos <- r, c
            | _ -> ()

    {
        Grid = grid;
        StartPos = startPos;
        EndPos = endPos;
    }

// Used for getting the 4 neighbors of a tile in the maze.
let getNbrs ((r, c)) = [
    r + 1, c;
    r, c + 1;
    r - 1, c;
    r, c - 1;
]

// Calculates the direction that the agent needs to have to get to the given position.
let getNeededDirection currPos nextPos = 
    let curR, curC = currPos
    let nxtR, nxtC = nextPos

    // Get the direction needed to move from 
    // `currPos` to `nextPos`.
    match nxtR - curR, nxtC - curC with
    | 1, 0 -> South
    | 0, 1 -> East
    | -1, 0 -> North
    | 0, -1 -> West
    | _ -> failwith "unreachable"

// Determines the agent's new direction along with the points 
// accumulated as a result of the turning.
let detTurn currPos nextPos currDir = 
    let neededDir = getNeededDirection currPos nextPos

    match (currDir, neededDir) with
    // No need to turn if already facing in the correct direction.
    | _ when currDir = neededDir -> neededDir, 0
    // Only 1 turn is needed, hence 1000 points.
    | East, (North | South)
    | West, (North | South)
    | North, (East | West)
    | South, (East | West) -> neededDir, 1000
    // Two turns are needed, hence 1000 * 2 points.
    | _ -> neededDir, 2000

// Gets the tile at the given position of the maze.
// Does not check if position is within bounds.
let getAtGrid ((r, c): Pos) (grid: Grid) = grid[r][c]

// Checks if the given position is traversable by the agent, i.e.
// it's not out of bounds or a wall tile ('#').
let posIsValid pos (grid: Grid) = 
    let r, c = pos
    
    let inBounds = r >= 0 
                          && r < grid.Length 
                          && c >= 0 
                          && c < grid[0].Length

    inBounds && getAtGrid pos grid <> '#'

// Explores a universe. Goes in all unexplored directions and calculates the 
// points accumulated over the course of the path.
let rec exploreUniverse input (solutions: byref<int list>) univ =
    // If the current universe reached the end position of the maze,
    // then the amount of points should be added to the solutions.
    if univ.CurrPos = input.EndPos then
        solutions <- univ.AccPoints :: solutions

    // Check all unexplored neighbors.
    for nbrPos in getNbrs univ.CurrPos do
        if not (univ.SeenPositions.Contains nbrPos) 
           && posIsValid nbrPos input.Grid 
        then
            let newDir, dirPoints = detTurn univ.CurrPos nbrPos univ.CurrDir
            let pointsToMove = 1

            // Explore the state created by moving to each neighbor,
            // adding up the necessary points.
            exploreUniverse
                input
                &solutions
                {
                    SeenPositions = univ.SeenPositions.Add univ.CurrPos
                    AccPoints = univ.AccPoints + dirPoints + pointsToMove
                    CurrPos = nbrPos
                    CurrDir = newDir
                }

let main () =
    let args = Environment.GetCommandLineArgs ()
    let filename = args[1] 
    let input = readInput filename
    
    // A list of possible solutions,
    // filled in (mutated) by exploring the various universes 
    // of possibilities.
    let mutable solutions = []

    exploreUniverse 
        input 
        &solutions 
        {        
            SeenPositions = Set.empty;
            AccPoints = 0;
            CurrPos = input.StartPos;
            CurrDir = East;
        }

    // Show the minimum solution.
    printfn "%d" (solutions |> List.min)

main ()