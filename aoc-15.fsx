// https://adventofcode.com/2020/day/15

open System
open System.Collections.Generic

let testInput = "0,3,6"
let input = "1,20,11,6,12,0"

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)

let toTurns =
    splitOnChar ',' >> Array.toList >> List.map int

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

let solution1test =
    testInput |> toTurns |> List.rev |> addNextTurns

let solution1 =
    input |> toTurns |> List.rev |> addNextTurns

// Part 2 - Immutable, and unfortunately a little slow. Finds solution in ~1.5 minutes
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
    input
    |> toTurns
    |> createState
    |> addNextTurns2 30_000_000

// Part 2 - Object programming, with full mutability
type Solver private () =
    let mutable count = 0
    let mutable lastSpoken = 0
    let previousSpoken = Dictionary<int, int>()

    let initialize () =
        count <- 0
        lastSpoken <- 0
        previousSpoken.Clear()

    let addToState (i: int) =
        if count > 0 then previousSpoken.Add(lastSpoken, count)
        count <- count + 1
        lastSpoken <- i

    let addNextTurn () =
        let thisTurn =
            match previousSpoken.TryGetValue(lastSpoken) with
            | true, lastPos -> count - lastPos
            | _ -> 0

        if previousSpoken.ContainsKey(lastSpoken) then
            previousSpoken.Item(lastSpoken) <- count
        else
            previousSpoken.Add(lastSpoken, count)

        count <- count + 1
        lastSpoken <- thisTurn

    let rec addNextTurns max =
        addNextTurn ()
        if count >= max then lastSpoken else addNextTurns max

    let addInitialState turns =
        initialize ()
        turns |> List.iter addToState
    with

        member private __.SolveInternal((initialTurns, max): int list * int) =
            initialTurns |> addInitialState
            addNextTurns max

        static member Solve((initialTurns, max): int list * int) =
            let solver = Solver()
            solver.SolveInternal(initialTurns, max)

let solution1test = Solver.Solve(testInput |> toTurns, 2020)
let solution1 = Solver.Solve(input |> toTurns, 2020)

let solution2test =
    Solver.Solve(testInput |> toTurns, 30_000_000)

let solution2 =
    Solver.Solve(input |> toTurns, 30_000_000)
