open System

type Pos = (int * int)

type Input = 
    Input of
        grid: char array array
        * startPos: Pos
        * endPos: Pos

type Direction = 
    | East
    | North
    | West
    | South

type Universe = 
    Universe of 
        seenPositions: Pos Set
        * accPoints: int
        * currPos: Pos
        * currDir: Direction

let readInput filename = 
    let grid = 
        IO.File.ReadAllLines filename
            |> Array.map (fun s -> s.ToCharArray())

    let mutable startPos = -1, -1
    let mutable endPos = -1, -1

    for r in 0..grid.Length - 1 do
        for c in 0..grid.Length - 1 do
            match grid[r][c] with
            | 'S' -> 
                startPos <- r, c
            | 'E' -> 
                endPos <- r, c
            | _ -> ()

    Input (grid, startPos, endPos)

let exploreUniverse (input: Input) (solutions: byref<int list>) (currUniv: Universe) =
    ()

let main () =
    let args = Environment.GetCommandLineArgs ()
    let filename = args[1] 
    let input = readInput filename

    //let mutable solutions = []
    //exploreUniverse input &solutions (Universe (...))
    ()

main ()