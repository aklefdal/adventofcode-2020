// https://adventofcode.com/2020/day/10

let input =
    "aoc-10-input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map int

let max = input |> Array.max

let counts =
    input
    |> Array.append [| 0; max + 3 |]
    |> Array.sort
    |> Array.pairwise
    |> Array.map (fun (x, y) -> y - x)
    |> Array.countBy id
    |> dict

let solution1 = (counts.[1]) * (counts.[3])