// https://adventofcode.com/2020/day/4

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let replace (old: string) (n: string) (s: string) = s.Replace(old, n)
let splitOnChar (c: char) (s: string) = s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
let splitOnString (delimiter: string) (s: string) = s.Split([|delimiter|], StringSplitOptions.RemoveEmptyEntries)
let newLine = "\r\n"
let entryDelimiter = newLine + newLine
let input =
    "aoc-04-input.txt" |> System.IO.File.ReadAllText

let entries =
    input
    |> splitOnString entryDelimiter
    |> Array.map (replace newLine " ") 

let parsePassport (entry: string) =
    entry
    |> splitOnChar ' '
    |> Array.map (fun t ->
        t
        |> splitOnChar ':'
        |> fun a -> a.[0], a.[1])
    |> dict

let validateRequiredFieldsPresent (p: IDictionary<string, string>) : bool =
    ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]
    |> List.forall p.ContainsKey

//
// Part 1
//
let solution1 =
    entries
    |> Array.map parsePassport
    |> Array.filter validateRequiredFieldsPresent
    |> Array.length

//
// Part 2
//
let colourRegex = Regex("^#[0-9a-f]{6}$")
let nineDigitsRegex = Regex("^[0-9]{9}$")
let isIntBetween min max (value: string) : bool =
    let valid, parsedValue = Int32.TryParse(value)
    valid && min <= parsedValue && parsedValue <= max
   
let validateValue (kvp: KeyValuePair<string, string>) : bool =
    match kvp.Key with
    | "byr" -> kvp.Value |> isIntBetween 1920 2002
    | "iyr" -> kvp.Value |> isIntBetween 2010 2020
    | "eyr" -> kvp.Value |> isIntBetween 2020 2030 
    | "hgt" ->
        let v = kvp.Value
        let length = v.Length
        let n = v.Substring(0, length - 2)
        let u = v.Substring(length - 2)
        match u with
        | "cm" -> n |> isIntBetween 150 193
        | "in" -> n |> isIntBetween 59 76
        | _ -> false
    | "hcl" -> colourRegex.IsMatch(kvp.Value)
    | "ecl" ->
        ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
        |> List.contains kvp.Value 
    | "pid" ->
        nineDigitsRegex.IsMatch(kvp.Value)        
    | _ -> true

let validatePassportValues (passport: IDictionary<string, string>) : bool =
    passport
    |> Seq.forall validateValue 

let solution2 =
    entries
    |> Array.map parsePassport
    |> Array.filter validateRequiredFieldsPresent
    |> Array.filter validatePassportValues
    |> Array.length
