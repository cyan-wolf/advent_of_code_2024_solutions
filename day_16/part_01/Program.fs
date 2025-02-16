open System

let readInput filename = IO.File.ReadAllLines filename

let main () = 
    let args = Environment.GetCommandLineArgs ()

    let lines = readInput args[1]

    let mutable startPos = -1, -1
    let mutable endPos = -1, -1

    for r in 0..lines.Length - 1 do
        for c in 0..lines[0].Length - 1 do
            match lines[r][c] with
            | 'S' -> 
                startPos <- r, c
            | 'E' -> 
                endPos <- r, c
            | _ -> ()

    printfn "%A" startPos
    printfn "%A" endPos

    // TODO
    // ...

main ()