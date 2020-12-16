// https://adventofcode.com/2020/day/16

open System
open System.Text.RegularExpressions

let splitOnString (delimiter: string) (s: string) =
    s.Split([| delimiter |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let newLine = "\r\n"
let entryDelimiter = newLine + newLine

let input =
    "aoc-16-input.txt"
    |> System.IO.File.ReadAllText
    |> splitOnString entryDelimiter

let (fieldsInput, myTicketInput, nearbyTicketInput) =
    match input with
    | [ fieldsInput; myTicketInput; nearbyTicketInput ] -> fieldsInput, myTicketInput, nearbyTicketInput
    | _ -> failwith "ouch"

let fieldRegex =
    Regex("^([ a-z]+): (\d+)-(\d+) or (\d+)-(\d+)$")

type Field =
    { FieldName: string
      From1: int
      To1: int
      From2: int
      To2: int }

let parseField s =
    let g = fieldRegex.Match(s).Groups

    { FieldName = g.[1].Value
      From1 = g.[2].Value |> int
      To1 = g.[3].Value |> int
      From2 = g.[4].Value |> int
      To2 = g.[5].Value |> int }

let fields =
    fieldsInput
    |> splitOnString newLine
    |> List.map parseField

let myTicket =
    myTicketInput
    |> splitOnString newLine
    |> List.skip 1
    |> List.head
    |> splitOnChar ','
    |> List.map int

let nearbyTickets =
    nearbyTicketInput
    |> splitOnString newLine
    |> List.skip 1
    |> List.map ((splitOnChar ',') >> List.map int)
    
let isValidForField value field =
    (field.From1 <= value && value <= field.To1)
    || (field.From2 <= value && value <= field.To2)

let isValidValue fields value =
    fields |> List.exists (isValidForField value)

let findInvalidValues fields values =
    values
    |> List.filter ((isValidValue fields) >> not)

[7;3;47] |> findInvalidValues fields
[40;4;50] |> findInvalidValues fields
[55;2;20] |> findInvalidValues fields
[38;6;12] |> findInvalidValues fields

let solution1 =
    nearbyTickets
    |> List.map (findInvalidValues fields)
    |> List.concat
    |> List.sum