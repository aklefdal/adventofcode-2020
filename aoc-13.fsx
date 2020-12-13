// https://adventofcode.com/2020/day/13

let input1 =
    "aoc-13-input.txt"
    |> System.IO.File.ReadAllLines

let input = [| "939"; "7,13,x,x,59,x,31,19" |]

let splitOnChar (c: char) (s: string) = s.Split([| c |], System.StringSplitOptions.RemoveEmptyEntries)

// Part 1
let t0 = input.[0] |> int

let busIds =
    input.[1]
    |> splitOnChar ','
    |> Array.map System.Int32.TryParse
    |> Array.filter fst
    |> Array.map snd

let findWaitTime t0 busId =
    busId, busId - (t0 % busId)

let waitTimes = busIds |> Array.map (findWaitTime t0)

let earliestBus = waitTimes |> Array.minBy snd
let solution1 =
    let (busId, waitTime) = earliestBus
    busId * waitTime
 