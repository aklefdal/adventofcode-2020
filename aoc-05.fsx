// https://adventofcode.com/2020/day/5

let input = "aoc-05-input.txt" |> System.IO.File.ReadAllLines

let takeNextHalf (low, high) c =
    let half = (high + 1 - low) / 2
    match c with
    | 'F' | 'L' -> low, (low + half - 1)
    | 'B' | 'R' -> (high - half + 1), high
    | _ -> failwith "Ouch"

let findSeatId (s: string) =
    let row = s.Substring(0, 7) |> Seq.fold takeNextHalf (0, 127) |> fst
    let col = s.Substring(7) |> Seq.fold takeNextHalf (0, 7) |> fst
    row * 8 + col

let seatIds = input |> Array.map findSeatId

let solution1 = seatIds |> Array.max

// Part 2
seatIds
|> Array.sort
|> Array.pairwise
|> Array.find (fun (f, s) -> s - f = 2)
|> fun (f, _) -> f + 1
