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
    if withNewTurn |> List.length >= 2020 then withNewTurn |> List.head else withNewTurn |> addNextTurns

let solution1 = initialTurns |> List.rev |> addNextTurns

// Part 2
type State =
    { Count: int
      PreviousSpoken: Map<int, int>
      LastSpoken: int }

let addNextTurn2 (state: State): State =
    let thisTurn =
        state.PreviousSpoken
        |> Map.tryFind state.LastSpoken
        |> Option.map (fun lastPos -> state.Count - lastPos)
        |> Option.defaultValue 0

    { Count = state.Count + 1
      PreviousSpoken =
          state.PreviousSpoken
          |> Map.add state.LastSpoken state.Count
      LastSpoken = thisTurn }

let rec addNextTurns2 max (state: State): int =
    let withNewTurn = state |> addNextTurn2
    if withNewTurn.Count >= max then withNewTurn.LastSpoken else withNewTurn |> addNextTurns2 max

let addToState state i =
    { Count = state.Count + 1
      PreviousSpoken =
          if state.Count = 0 then
              state.PreviousSpoken
          else
              state.PreviousSpoken
              |> Map.add state.LastSpoken state.Count
      LastSpoken = i }

let createState turns =
    turns
    |> List.fold
        addToState
           { Count = 0
             PreviousSpoken = Map.empty
             LastSpoken = 0 }

let solution2 =
    initialTurns
    |> createState
    |> addNextTurns2 30_000_000
