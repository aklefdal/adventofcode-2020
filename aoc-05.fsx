// https://adventofcode.com/2020/day/5

let input = "aoc-05-input.txt" |> System.IO.File.ReadAllLines

let takeLowerHalf (low, high) =
    let newHigh = low + (high + 1 - low) / 2 - 1
    low, newHigh

let takeHigherHalf (low, high) =
    let newLow = high - (high + 1 - low) / 2 + 1
    newLow, high

let takeNextHalf state c =
    match c with
    | 'F' | 'L' -> state |> takeLowerHalf
    | 'B' | 'R' -> state |> takeHigherHalf
    | _ -> failwith "Ouch"

let findSeatId (s: string) =
    let row =
        s.Substring(0, 7)
        |> Seq.fold takeNextHalf (0, 127)
        |> fst

    let col =
        s.Substring(7)
        |> Seq.fold takeNextHalf (0, 7)
        |> fst

    row * 8 + col

let seatIds = input |> Array.map findSeatId

let solution1 = seatIds |> Seq.max

//
// Part 2
//
seatIds
|> Array.sort
|> Array.pairwise
|> Array.filter (fun (f, s) -> s - f = 2)
|> Array.head
|> fun (f, _) -> f + 1