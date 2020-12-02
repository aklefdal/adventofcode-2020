// https://adventofcode.com/2020/day/2

open System
open System.IO

//
// Input
//
let loadInput () = "aoc-02-input.txt" |> File.ReadAllLines

type PasswordPolicy =
    { FirstInt: int
      SecondInt: int
      Char: char
      Password: string }

let parse (s: string): PasswordPolicy =
    let a =
        s.Split([| '-'; ' '; ':' |], StringSplitOptions.RemoveEmptyEntries)

    { FirstInt = a.[0] |> int
      SecondInt = a.[1] |> int
      Char = a.[2].[0]
      Password = a.[3] }

//
// Part 1
//
let isValid (entry: PasswordPolicy): bool =
    let count =
        entry.Password.ToCharArray()
        |> Array.filter (fun c -> c = entry.Char)
        |> Array.length

    entry.FirstInt <= count && count <= entry.SecondInt

let solve1 =
    Array.map parse
    >> Array.filter isValid
    >> Array.length

#time "on"
let solution1 = loadInput() |> solve1
#time "off"

//
// Part 2
//
let isCharAtPosition (c: char) (i: int) (s: string): bool =
    if s.Length < i then
        false
    else
        s.Chars(i - 1) = c

let isValid2 (entry: PasswordPolicy): bool =
    // Using <> as XOR-operator: true if one is true and one is , false if both are true or both are false
    (isCharAtPosition entry.Char entry.FirstInt entry.Password)
    <> (isCharAtPosition entry.Char entry.SecondInt entry.Password)

let solve2 = 
    Array.map parse
    >> Array.filter isValid2
    >> Array.length

#time "on"
let solution2 = loadInput() |> solve2
#time "off"

//
//// Part 1 using regex
//open System.Text.RegularExpressions
//
//let rgx = Regex(@"^(\d{1,2})-(\d{1,2}) ([a-z]): (\w*)$")
//
//let parseUsingRegex (s: string): PasswordPolicy =
//    let g = rgx.Match(s).Groups
//    { FirstInt = g.[1].Value |> int
//      SecondInt = g.[2].Value |> int
//      Char = g.[3].Value.[0]
//      Password = g.[4].Value }
//
//let solve1b =
//    Array.map parseUsingRegex
//    >> Array.filter isValid
//    >> Array.length
//
//#time "on"
//let solution1b = loadInput() |> solve1b
//#time "off"
