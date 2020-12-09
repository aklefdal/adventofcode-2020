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
