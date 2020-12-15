// https://adventofcode.com/2020/day/5

open System

let input = "1,20,11,6,12,0"

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)

let initialTurns =
    input
    |> splitOnChar ','
    |> Array.toList
    |> List.map int
    |> List.rev

let addNextTurn turns =
    match turns with
    | [] -> failwith "ouch?!?!?"
    | lastNumberSpoken :: previousSpoken ->
        let thisTurn =
            previousSpoken
            |> List.tryFindIndex ((=) lastNumberSpoken)
            |> Option.map ((+) 1)
            |> Option.defaultValue 0
        thisTurn :: turns
        
let rec addNextTurns turns =
    let withNewTurn = turns |> addNextTurn
    if withNewTurn |> List.length >= 2020 then
        withNewTurn |> List.head
    else
        withNewTurn |> addNextTurns

let solution1 = initialTurns |> addNextTurns 