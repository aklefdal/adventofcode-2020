// https://adventofcode.com/2020/day/9

let input =
    "aoc-09-input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map int64

let preambleLength = 25

let validate n =
    seq {
        for i = n - preambleLength to n - 1 do
            for j = i + 1 to n - 1 do
                input.[i] + input.[j]
    }
    |> Seq.contains input.[n]

let rec findError n =
    match n |> validate with
    | false -> input.[n]
    | _ -> n + 1 |> findError

let solution1 = preambleLength |> findError

// Part 2
let rec checkSum windowSize =
    let contiguousSet =
        input
        |> Array.windowed windowSize
        |> Array.filter (fun a -> a |> Array.sum = solution1)
        |> Array.tryHead
    match contiguousSet with
    | Some a -> (a |> Array.min) + (a |> Array.max)
    | None -> (windowSize + 1) |> checkSum
    
let solution2 = 2 |> checkSum