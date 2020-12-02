// https://adventofcode.com/2020/day/2

open System
open System.IO

//
// Input
//
let loadInput () = "aoc-02-input.txt" |> File.ReadAllLines

type Entry =
    { FirstInt: int
      SecondInt: int
      Char: char
      Password: string }

let parseEntry (s: string): Entry =
    let a =
        s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)

    { FirstInt = a.[0] |> int
      SecondInt = a.[1] |> int
      Char = a.[2].[0]
      Password = a.[3] }

//
// Part 1
//
let isValid (entry: Entry): bool =
    let count =
        entry.Password
        |> Seq.filter (fun c -> c = entry.Char)
        |> Seq.length

    entry.FirstInt <= count && count <= entry.SecondInt

let solve1 input =
    input
    |> Seq.map parseEntry
    |> Seq.filter isValid
    |> Seq.length

#time "on"
let solution1 = loadInput() |> solve1
#time "off"

//
// Part 2
//
let isCharAtPosition (s: string) c pos =
    pos <= s.Length && s.[pos - 1] = c

let isValid2 (entry: Entry): bool =
    // Using <> as XOR-operator: true if one is true and one is false, false if both are true or both are false
    (isCharAtPosition entry.Password entry.Char entry.FirstInt)
    <> (isCharAtPosition entry.Password entry.Char entry.SecondInt)

let solve2 input =
    input
    |> Seq.map parseEntry
    |> Seq.filter isValid2
    |> Seq.length

#time "on"
let solution2 = loadInput() |> solve2
#time "off"

//
//// Part 1 using regex
//open System.Text.RegularExpressions
//
//let rgx = Regex(@"^(\d{1,2})-(\d{1,2}) ([a-z]): (\w*)$")
//
//let parseUsingRegex (s: string): Entry =
//    let g = rgx.Match(s).Groups
//    { FirstInt = g.[1].Value |> int
//      SecondInt = g.[2].Value |> int
//      Char = g.[3].Value.[0]
//      Password = g.[4].Value }
//
//let solve1b input =
//    input
//    |> Seq.map parseUsingRegex
//    |> Seq.filter isValid
//    |> Seq.length
//
//#time "on"
//let solution1b = loadInput() |> solve1b
//#time "off"
