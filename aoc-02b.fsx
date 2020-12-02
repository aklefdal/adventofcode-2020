// https://adventofcode.com/2020/day/2
// More terse solution

open System
open System.IO

//
// Input
//
let loadInput () = "aoc-02-input.txt" |> File.ReadAllLines

//
// Part 1
//
let isValid (s: string): bool =
    let a = s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)
    let letter = a.[2].[0]
    let count = a.[3] |> Seq.sumBy (fun c -> if c = letter then 1 else 0)

    (a.[0] |> int) <= count && count <= (a.[1] |> int)

let solution1 =
    loadInput ()
    |> Seq.filter isValid
    |> Seq.length

//
// Part 2
//
let isCharAtPosition (c: char) (i: int) (s: string): bool =
    if s.Length < i then false else s.Chars(i - 1) = c

let isValid2 (s: string): bool =
    let a = s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)

    let letter = a.[2].[0]
    let password = a.[3]
    (isCharAtPosition letter (a.[0] |> int) password)
    <> (isCharAtPosition letter (a.[1] |> int) password)

let solution2 =
    loadInput ()
    |> Seq.filter isValid2
    |> Seq.length
