// https://adventofcode.com/2020/day/14

open System
open System.IO
open System.Text.RegularExpressions

let input = "aoc-14-input.txt" |> File.ReadAllLines

type Command =
    | Mask of char []
    | Mem of int64 * int64

let bitArrayToInt64 (cs: char []) =
    cs
    |> String
    |> fun s -> Convert.ToInt64(s.ToString(), 2)

let int64ToBitArray (i: int64) =
    Convert.ToString(i, 2).PadLeft(36, '0').ToCharArray()

let memRegex = Regex("^mem\[(\d+)\] = (\d+)$")

let parseCommand (s: string): Command =
    if s.Substring(0, 4) = "mask" then
        let mask = s.Substring(7).ToCharArray()
        Mask mask
    else
        let groups = memRegex.Match(s).Groups
        let memSlot = groups.[1].Value |> int64
        let value = groups.[2].Value |> int64
        Mem (memSlot, value)

// Part 1
let applyMask mask value =
    value
    |> int64ToBitArray
    |> Array.zip mask
    |> Array.map (fun (m, b) -> if m = 'X' then b else m)
    |> bitArrayToInt64

let processCommand ((values, currentMask): Map<int64, int64> * char []) command =
    match command with
    | Mask mask -> values, mask
    | Mem (pos, value) ->
        let maskedValue = value |> applyMask currentMask
        values.Add(pos, maskedValue), currentMask

let solution1 =
    input
    |> Array.map parseCommand
    |> Array.fold processCommand (Map.empty, [||])
    |> fst
    |> Seq.sumBy (fun kvp -> kvp.Value)

// Part 2
let setBit (cs: char []) i c =
    // Deep copy
    let s = cs |> String
    let a = s.ToCharArray()
    // Mutate
    a.[i] <- c    
    a

let rec expandFloatingBits bitArray =
    match bitArray |> Array.tryFindIndex ((=) 'X') with
    | Some i ->
        [ '0'; '1' ]
        |> List.map (setBit bitArray i)
        |> List.map expandFloatingBits
        |> List.concat
    | None -> [ bitArray |> bitArrayToInt64 ]

let applyMemoryMask mask memSlot =
    memSlot
    |> int64ToBitArray
    |> Array.zip mask
    |> Array.map (fun (m, b) ->
        match m with
        | '0' -> b
        | 'X' -> 'X'
        | '1' -> '1'
        | _ -> failwith "ouchhh")
    |> expandFloatingBits

let addToValueMap value (valueMap: Map<int64, int64>) memPos =
    valueMap.Add(memPos, value)

let processCommand2 (valueMap, currentMask) command =
    match command with
    | Mask mask -> valueMap, mask
    | Mem (pos, value) ->
        let memPositions = pos |> (applyMemoryMask currentMask)

        let updatedMap =
            memPositions |> List.fold (addToValueMap value) valueMap

        updatedMap, currentMask

let solution2 =
    input
    |> Array.map parseCommand
    |> Array.fold processCommand2 (Map.empty, [||])
    |> fst
    |> Seq.sumBy (fun kvp -> kvp.Value)
