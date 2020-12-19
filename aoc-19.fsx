// https://adventofcode.com/2020/day/

open System
open System.Collections.Generic
open System.IO

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let splitOnString (delimiter: string) (s: string) =
    s.Split([| delimiter |], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
let trim (s:string) = s.Trim()
let newLine = "\r\n"
let entryDelimiter = newLine + newLine

let input = "aoc-19-input.txt" |> File.ReadAllText

let inputParts = input |> splitOnString entryDelimiter

// Part 1

type AndRule =
    | Str of string
    | LinkedRules of int list

let parseAndRule s =
    if s = "\"a\"" then
        Str "a"
    elif s = "\"b\"" then
        Str "b"
    else
        s
        |> splitOnChar ' '
        |> List.map int
        |> LinkedRules

let parseRule (s: string) =
    let a = s |> splitOnString ": "
    let ruleNumber = a.[0] |> int

    let orRules =
        a.[1] |> splitOnChar '|' |> List.map parseAndRule
    ruleNumber, orRules

let crossProduct (a: string list) (b: string list) =
    seq {
        for el1 in a do
            for el2 in b do
                yield el1 + el2
    } |> List.ofSeq

let rec findPossibilitiesForAndRule (rules: IDictionary<int,AndRule list>) (rule: AndRule) : string list =
    match rule with
    | Str s -> [ s ]
    | LinkedRules ruleNumbers ->
        ruleNumbers
        |> List.map (findPossibilities rules)
        |> List.reduce crossProduct

and findPossibilities (rules: IDictionary<int,AndRule list>) (ruleNumber: int) : string list =
    rules.[ruleNumber] |> List.map (findPossibilitiesForAndRule rules) |> List.concat
    
//let rulesInput =
//     [ "0: 4 1 5"
//       "1: 2 3 | 3 2"
//       "2: 4 4 | 5 5"
//       "3: 4 5 | 5 4"
//       "4: \"a\""
//       "5: \"b\"" ]

let rules =
    inputParts.[0]
    |> splitOnString newLine
    |> List.map parseRule
    |> dict

let possibilities = 0 |> findPossibilities rules |> Set.ofList

let solution1 =
    inputParts.[1]
    |> splitOnString newLine
    |> List.filter (fun s -> possibilities |> Set.contains s)
    |> List.length
    
