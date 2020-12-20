// https://adventofcode.com/2020/day/19

open System
open System.Collections.Generic
open System.IO

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let splitOnString (delimiter: string) (s: string) =
    s.Split([| delimiter |], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let trim (s: string) = s.Trim()
let newLine = "\r\n"
let entryDelimiter = newLine + newLine

type RuleNumber = RuleNumber of int

type AndRule =
    | Character of char
    | LinkedRules of RuleNumber list

let parseAndRule s =
    if s = "\"a\"" then
        Character 'a'
    elif s = "\"b\"" then
        Character 'b'
    else
        s
        |> splitOnChar ' '
        |> List.map int
        |> List.map RuleNumber
        |> LinkedRules

let parseRule (s: string) =
    let a = s |> splitOnString ": "
    let ruleNumber = a.[0] |> int

    let orRules =
        a.[1] |> splitOnChar '|' |> List.map parseAndRule

    ruleNumber |> RuleNumber, orRules

// Part 1
module Part1 =

    let crossProduct (a: string list) (b: string list) =
        seq {
            for el1 in a do
                for el2 in b do
                    yield el1 + el2
        }
        |> List.ofSeq

    let rec findPossibilitiesForAndRule (rules: IDictionary<RuleNumber, AndRule list>) (rule: AndRule): string list =
        match rule with
        | Character c -> [ c.ToString() ]
        | LinkedRules ruleNumbers ->
            ruleNumbers
            |> List.map (findPossibilities rules)
            |> List.reduce crossProduct

    and findPossibilities (rules: IDictionary<RuleNumber, AndRule list>) (ruleNumber: RuleNumber): string list =
        rules.[ruleNumber]
        |> List.map (findPossibilitiesForAndRule rules)
        |> List.concat

    let inputParts =
        "aoc-19-input.txt"
        |> File.ReadAllText
        |> splitOnString entryDelimiter

    let rules =
        inputParts.[0]
        |> splitOnString newLine
        |> List.map parseRule
        |> dict

    let possibilities =
        0
        |> RuleNumber
        |> findPossibilities rules
        |> Set.ofList

    let solution1 =
        inputParts.[1]
        |> splitOnString newLine
        |> List.filter (fun s -> possibilities |> Set.contains s)
        |> List.length

// Part 2
module Part2 =

    let rec matchAndRule rules message (rule: AndRule): char [] option =
        match rule with
        | Character c ->
            if message |> Array.length > 0 && message.[0] = c then
                message.[1..] |> Some
            else
                None
        | LinkedRules ruleNumbers ->
            match ruleNumbers with
            | [] -> Some message
            | ruleNumber :: rest ->
                ruleNumber
                |> matchRule rules message
                |> Option.bind (fun s -> rest |> LinkedRules |> (matchAndRule rules s))

    and matchRule (rules: IDictionary<RuleNumber,AndRule list>) (message: char []) (ruleNumber: RuleNumber) : char [] option =
        rules.[ruleNumber] |> List.tryPick (matchAndRule rules message)

    let inputParts =
        "aoc-19-input.txt"
        |> File.ReadAllText
        |> splitOnString entryDelimiter

    let rules =
        inputParts.[0]
        |> splitOnString newLine
        |> List.map parseRule
        |> List.filter (fun (i, _) -> i <> RuleNumber 8 && i <> RuleNumber 11)
        |> List.append [ "8: 42 | 42 8" |> parseRule; "11: 42 31 | 42 11 31" |> parseRule ]
        |> dict

    let findMatch rules (message: string) =
        RuleNumber 0 |> matchRule rules (message.ToCharArray())
    let solution2 =
        inputParts.[1]
        |> splitOnString newLine
        |> List.filter (fun s -> s |> findMatch rules |> Option.isSome)
        |> List.length
