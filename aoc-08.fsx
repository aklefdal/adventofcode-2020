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
    | Looping of int
    | Done of int

type ProgramResult =
    | LoopingProgram of int
    | CompletingProgram of int

let program = input |> Array.map parseLine

let (++) (state: State) ((a, p): int * int) : State =
    { Accumulator = state.Accumulator + a
      CurrentPosition = state.CurrentPosition + p }

let getNewState (program: (string * int)[]) (state: State): ProcessingResult =
    if state.CurrentPosition >= program.Length then
        state.Accumulator |> Done
    else
        match program.[state.CurrentPosition] with
        | ("acc", n) ->
            program.[state.CurrentPosition] <- ("done", 0)
            state ++ (n, 1) |> NewState
        | ("nop", _) ->
            program.[state.CurrentPosition] <- ("done", 0) 
            state ++ (0, 1) |> NewState
        | ("jmp", n) ->
            program.[state.CurrentPosition] <- ("done", 0) 
            state ++ (0, n) |> NewState
        | _ -> state.Accumulator |> Looping
    
let rec checkProgram (program: (string * int)[]) (currentState: State): ProgramResult =
    match currentState |> (getNewState program) with
    | Done res -> CompletingProgram res
    | Looping res -> LoopingProgram res
    | NewState state -> state |> (checkProgram program)
    
let solution1 = { CurrentPosition = 0; Accumulator = 0 } |> (checkProgram program)

// Part 2

let swap (program:(string * int)[]) (swapNo: int) : unit =
    match program.[swapNo] with
    | ("nop", n) -> program.[swapNo] <- ("jmp", n)
    | ("jmp", n) -> program.[swapNo] <- ("nop", n)
    | _ -> ()

let rec swapAndTry (swapNo: int) : int =
    let program = input |> Array.map parseLine
    swap program swapNo
    match checkProgram program { CurrentPosition = 0; Accumulator = 0 } with
    | CompletingProgram res -> res
    | LoopingProgram _ -> swapNo + 1 |> swapAndTry

let solution2 = 0 |> swapAndTry