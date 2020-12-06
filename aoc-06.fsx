// https://adventofcode.com/2020/day/6

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let replace (old: string) (n: string) (s: string) = s.Replace(old, n)
let splitOnChar (c: char) (s: string) = s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
let splitOnString (delimiter: string) (s: string) = s.Split([|delimiter|], StringSplitOptions.RemoveEmptyEntries)
let newLine = "\r\n"
let entryDelimiter = newLine + newLine
let input =
    "aoc-06-input.txt" |> System.IO.File.ReadAllText

let solution1 =
    input
    |> splitOnString entryDelimiter
    |> Seq.map (replace newLine "")
    |> Seq.map (fun s -> s |> Seq.groupBy id |> Seq.length)
    |> Seq.sum

// Part 2

let countForAll (group: string) =
    let peoplesAnswers = group |> splitOnString newLine
    let countOfPeopleInGroup = peoplesAnswers.Length
    let allAnswers = peoplesAnswers |> Array.reduce (+)
    let counts = allAnswers |> Seq.countBy id
    counts
    |> Seq.filter (fun (_, count) -> count = countOfPeopleInGroup)
    |> Seq.length
    

let solution2 =
    input
    |> splitOnString entryDelimiter
    |> Seq.map countForAll
    |> Seq.sum
