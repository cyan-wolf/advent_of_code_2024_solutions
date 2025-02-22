open System

type Pos = (int * int)
type Grid = char array array

type Input = {
    Grid: Grid
    StartPos: Pos
    EndPos: Pos
}

type Direction = 
    | East
    | North
    | West
    | South

type Universe = {
    SeenPositions: Pos Set
    AccPoints: int
    CurrPos: Pos
    CurrDir: Direction
}

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

    {
        Grid = grid;
        StartPos = startPos;
        EndPos = endPos;
    }

let getNbrs ((r, c)) = [
    r + 1, c;
    r, c + 1;
    r - 1, c;
    r, c - 1;
]

let getNeededDirection (currPos: Pos) (nextPos: Pos) = 
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

let detTurn (currPos: Pos) (nextPos: Pos) (currDir: Direction) = 
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

let getAtGrid ((r, c): Pos) (grid: Grid) = grid[r][c]

let posIsValid (pos: Pos) (grid: Grid) = 
    let r, c = pos
    
    let inBounds = r >= 0 
                          && r < grid.Length 
                          && c >= 0 
                          && c < grid[0].Length

    //printfn "%b, %A, %A" inBounds (getAtGrid pos grid) pos

    inBounds && getAtGrid pos grid <> '#'

let rec exploreUniverse 
    (input: Input) 
    (solutions: byref<int list>) 
    (univ: Universe) =

    // printfn "%A" univ.CurrPos

    if univ.CurrPos = input.EndPos then
        solutions <- univ.AccPoints :: solutions

    for nbrPos in getNbrs univ.CurrPos do
        //printfn "%b, %b, %A" (not (univ.SeenPositions.Contains nbrPos)) (posIsValid univ.CurrPos input.Grid ) (nbrPos)

        if not (univ.SeenPositions.Contains nbrPos) 
           && posIsValid nbrPos input.Grid 
        then
            let newDir, dirPoints = detTurn univ.CurrPos nbrPos univ.CurrDir
            let pointsToMove = 1

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

    printfn "%d" (solutions |> List.min)

main ()