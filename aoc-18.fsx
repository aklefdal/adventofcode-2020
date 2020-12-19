// https://adventofcode.com/2020/day/18

open System
open System.IO

let input =
    "aoc-18-input.txt"
    |> File.ReadAllLines
    |> Array.toList

let splitOnChar (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let splitOnChars (cs: string) (s: string) =
    s.Split(cs.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let rec calculateSimpleTokens (ts: string list): int64 =
    match ts with
    | [] -> failwith "What? no token"
    | [ t ] -> t |> int64
    | [ _; _ ] -> failwith "What? two tokens"
    | operand1 :: operator :: operand2 :: rest ->
        let n1 = operand1 |> int64
        let n2 = operand2 |> int64

        let sum =
            match operator with
            | "+" -> n1 + n2
            | "*" -> n1 * n2
            | _ -> failwith "what kinda token is this"

        sum.ToString() :: rest |> calculateSimpleTokens

let calculateSimpleString s =
    s
    |> splitOnChar ' '
    |> calculateSimpleTokens
    |> sprintf "%i"

let rec calculateCompoundString calculateSimpleString (s: string): string =
    match s.IndexOf '(', s.IndexOf ')' with
    | -1, -1 -> s |> calculateSimpleString
    | -1, endP ->
        let compoundPart =
            s.Substring(0, endP) |> calculateSimpleString

        let endPart = s.Substring(endP)
        compoundPart + endPart
    | startP, endP ->
        let nextStartP = s.Substring(startP + 1).IndexOf('(')
        if nextStartP >= 0 && startP + 1 + nextStartP < endP then
            let interimPart =
                s.Substring(startP + 1 + nextStartP)
                |> (calculateCompoundString calculateSimpleString)

            let firstPart = s.Substring(0, startP + 1 + nextStartP)

            firstPart + interimPart
            |> (calculateCompoundString calculateSimpleString)
        else
            let firstPart =
                if startP > 0 then s.Substring(0, startP) else ""

            let compoundPart =
                s.Substring(startP + 1, endP - startP - 1)
                |> calculateSimpleString

            let endPart = s.Substring(endP + 1)
            firstPart + compoundPart + endPart

let rec calculate s =
    let res = s |> (calculateCompoundString calculateSimpleString)

    match res |> Int64.TryParse with
    | true, value -> value
    | false, _ -> res |> calculate

let solution1 = input |> List.map calculate |> List.sum

// Part 2

let rec calculateAdditions (previous : string list) (ts: string list) : string list =
    match ts with
    | [] -> failwith "What? no token"
    | [ t ] -> previous @ [ t ]  
    | [ _; _ ] -> failwith "What? two tokens"
    | operand1 :: operator :: operand2 :: rest when operator = "+" ->
        let n1 = operand1 |> int64
        let n2 = operand2 |> int64
        let sum =  n1 + n2
        (sum.ToString() :: rest) |> calculateAdditions previous
    | operand1 :: operator :: operand2 :: rest when operator = "*" ->
        calculateAdditions (previous @ [ operand1; operator ]) (operand2 :: rest)
    | _ ->
        failwith "what kind of operator is that"

let rec calculateMultiplication (ts: string list) : string =
    match ts with
    | [] -> failwith "What? no token"
    | [ t ] -> t
    | [ _; _ ] -> failwith "What? two tokens"
    | operand1 :: operator :: operand2 :: rest when operator = "*" ->
        let n1 = operand1 |> int64
        let n2 = operand2 |> int64
        let product =  n1 * n2
        (product.ToString() :: rest) |> calculateMultiplication
    | _ ->
        failwith "what kind of operator is that"
        
let calculateSimpleString2 s =
    s
    |> splitOnChar ' '
    |> calculateAdditions []
    |> calculateMultiplication

let rec calculate2 s =
    let res = s |> (calculateCompoundString calculateSimpleString2)
    match res |> Int64.TryParse with
    | true, value -> value
    | false, _ -> res |> calculate2

let solution2 = input |> List.map calculate2 |> List.sum
