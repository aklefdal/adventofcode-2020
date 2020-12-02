// https://adventofcode.com/2020/day/2
// More terse solution

open System
open System.IO

#time "on"
let input = "aoc-02-input.txt" |> File.ReadAllLines

// Part 1
let isValid (s: string): bool =
    let a =
        s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)
    let letter = a.[2].[0]
    let count =
        seq { for c in a.[3] do if c = letter then yield c } 
        |> Seq.length

    (a.[0] |> int) <= count && count <= (a.[1] |> int)

let solution1 =
    input
    |> Array.filter isValid
    |> Array.length

#time "off"

// Part 2
let isCharAtPosition (c: char) (i: int) (s: string): bool =
    if s.Length < i then
        false
    else
        s.Chars(i - 1) = c

let isValid2 (s: string): bool =
    let a =
        s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)
    let letter = a.[2].[0]
    let password = a.[3]
    (isCharAtPosition letter (a.[0] |> int) password)
    <> (isCharAtPosition letter (a.[1] |> int) password)

#time "on"
let solution2 =
    input
    |> Array.filter isValid2
    |> Array.length
#time "off"
