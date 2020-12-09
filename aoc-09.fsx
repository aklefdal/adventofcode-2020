// https://adventofcode.com/2020/day/9

let input =
    "aoc-09-input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map int64

let preambleLength = 25

let getValidSums n =
    let preamble = input.[(n - preambleLength)..(n - 1)]
    seq {
        for i = 0 to preambleLength - 1 do
            for j = i + 1 to preambleLength - 1 do
                preamble.[i] + preamble.[j]
    }
    |> Seq.toArray

let validate n =
    n |> getValidSums |> Array.contains input.[n]

let rec findError n =
    match n |> validate with
    | false -> input.[n]
    | _ -> n + 1 |> findError

let solution1 = 25 |> findError