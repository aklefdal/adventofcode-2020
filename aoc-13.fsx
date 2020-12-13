// https://adventofcode.com/2020/day/13

let input =
    "aoc-13-input.txt" |> System.IO.File.ReadAllLines

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], System.StringSplitOptions.RemoveEmptyEntries)

// Part 1
let t0 = input.[0] |> int64

let busIds =
    input.[1]
    |> splitOnChar ','
    |> Array.map System.Int64.TryParse
    |> Array.filter fst
    |> Array.map snd

let findWaitTime t0 busId = busId, busId - (t0 % busId)

let waitTimes = busIds |> Array.map (findWaitTime t0)

let earliestBus = waitTimes |> Array.minBy snd

let solution1 =
    let (busId, waitTime) = earliestBus
    busId * waitTime

// Part 2
let buses =
    input.[1]
    |> splitOnChar ','
    |> Array.toList
    |> List.mapi (fun i s -> int64 i, s)
    |> List.filter (fun (_, s) -> s <> "x")
    |> List.map (fun (i, s) -> i, s |> System.Int64.Parse)
    |> List.sortBy snd

let rec isValidTimeForBus (currentTime, increment) (offset, busId) =
    if (currentTime + offset) % busId = 0L then
        let newIncrement = increment * busId
        (currentTime, newIncrement)
    else
        isValidTimeForBus (currentTime + increment, increment) (offset, busId)

let (firstBus, remainingBuses) =
    match buses with
    | [] -> failwith "ouch"
    | firstBus :: remainingBuses -> firstBus, remainingBuses 

let solution2 =
    remainingBuses
    |> List.fold isValidTimeForBus (0L, firstBus |> snd)
    |> fst