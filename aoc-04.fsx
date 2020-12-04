// https://adventofcode.com/2020/day/4

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let replace (old: string) (n: string) (s: string) = s.Replace(old, n)
let split (chars: char []) (s: string) = s.Split(chars)
let newLine = "\r\n"

let input =
    "aoc-04-input.txt" |> System.IO.File.ReadAllText

// This is bad, I'm ashamed
let entries =
    input
    |> replace newLine " " // Put everything on one line
    |> replace "  " "\t" // two newlines became two spaces, replace again with tab-character
    |> split [| '\t' |] // Split into entries by tab-character
let parsePassport (entry: string) =
    entry
    |> split [| ' ' |]
    |> Array.map (fun t ->
        t
        |> split [| ':' |]
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

let validateValue (kvp: KeyValuePair<string, string>) : bool =
    match kvp.Key with
    | "byr" ->
        let valid, birthYear = Int32.TryParse(kvp.Value)
        valid && 1920 <= birthYear && birthYear <= 2002
    | "iyr" ->
        let valid, issueYear = Int32.TryParse(kvp.Value)
        valid && 2010 <= issueYear && issueYear <= 2020
    | "eyr" -> 
        let valid, expirationYear = Int32.TryParse(kvp.Value)
        valid && 2020 <= expirationYear && expirationYear <= 2030
    | "hgt" ->
        let v = kvp.Value
        let length = v.Length
        let n = v.Substring(0, length - 2)
        let u = v.Substring(length - 2)
        match u with
        | "cm" ->
            let valid, heightInCm = Int32.TryParse(n)
            valid && 150 <= heightInCm && heightInCm <= 193
        | "in" ->
            let valid, heightInInches = Int32.TryParse(n)
            valid && 59 <= heightInInches && heightInInches <= 76
        | _ -> false
    | "hcl" ->
        colourRegex.IsMatch(kvp.Value)
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
