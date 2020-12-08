// https://adventofcode.com/2020/day/8

let splitOnString (delimiter: string) (s: string) =
    s.Split([| delimiter |], System.StringSplitOptions.RemoveEmptyEntries)

let input =
    "aoc-08-input.txt" |> System.IO.File.ReadAllLines

let replace (old: string) (n: string) (s: string) = s.Replace(old, n)

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], System.StringSplitOptions.RemoveEmptyEntries)

let parseLine (s: string) =
    let a = s |> splitOnChar ' '
    a.[0], int a.[1]

type State =
    { CurrentPosition: int
      Accumulator: int }

type ProcessingResult =
    | NewState of State
    | Done of int

let program =
    "aoc-08-input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map parseLine

let getNewState (currentState: State): ProcessingResult =
    match program.[currentState.CurrentPosition] with
    | ("acc", n) ->
        program.[currentState.CurrentPosition] <- ("done", 0) 
        { Accumulator = currentState.Accumulator + n
          CurrentPosition = currentState.CurrentPosition + 1 }
        |> NewState
    | ("nop", _) ->
        program.[currentState.CurrentPosition] <- ("done", 0) 
        { currentState with
              CurrentPosition = currentState.CurrentPosition + 1 }
        |> NewState
    | ("jmp", n) ->
        program.[currentState.CurrentPosition] <- ("done", 0) 
        { currentState with
              CurrentPosition = currentState.CurrentPosition + n }
        |> NewState
    | _ -> currentState.Accumulator |> Done
    
let rec getAccumulator (currentState: State): int =
    match currentState |> getNewState with
    | Done res -> res
    | NewState state -> state |> getAccumulator
    
let solution1 = { CurrentPosition = 0; Accumulator = 0 } |> getAccumulator

// Part 2
