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
    | fieldsInput :: myTicketInput :: nearbyTicketInput :: _ -> fieldsInput, myTicketInput, nearbyTicketInput
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
    |> List.toArray

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

let solution1 =
    nearbyTickets
    |> List.map (findInvalidValues fields)
    |> List.concat
    |> List.sum

// part 2
let isValidTicket fields values =
    values |> List.forall (isValidValue fields)

let validNearbyTickets =
    nearbyTickets
    |> List.filter (isValidTicket fields)

let findValidPositions (field: Field) (ticket: int list): bool list =
    ticket
    |> List.map (fun value -> isValidForField value field)

let findValidFieldPositions (tickets: int list list) (field: Field): (string * int list) =
    let rec checkPosition (validations: bool list list) (pos: int) (positions: int list) =
        if pos >= validations.Head.Length then
            positions
        else
            let isValidPos =
                validations
                |> List.map (List.skip pos)
                |> List.forall (List.head)

            if isValidPos
            then checkPosition validations (pos + 1) (pos :: positions)
            else checkPosition validations (pos + 1) (positions)

    let validatedTickets =
        tickets |> List.map (findValidPositions field)

    let validPositions = checkPosition validatedTickets 0 []
    field.FieldName, validPositions

let allocatePosition (takenPositions: (string * int) list) ((fieldName, validPositions): string * int list) =
    let position =
        validPositions
        |> List.filter (fun i ->
            takenPositions
            |> List.map snd
            |> List.contains i
            |> not)
        |> List.head

    (fieldName, position) :: takenPositions

let solution2 =
    let fieldsPositions =
        fields
        |> List.map (findValidFieldPositions validNearbyTickets)
        |> List.sortBy (fun (_, ns) -> ns.Length)
        |> List.fold allocatePosition []
    
    fieldsPositions
    |> List.filter (fun (name, _) -> name.StartsWith("departure"))
    |> List.map snd
    |> List.map (fun pos -> myTicket.[pos] |> int64)
    |> List.reduce (*)
