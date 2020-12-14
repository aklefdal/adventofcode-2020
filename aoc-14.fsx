// https://adventofcode.com/2020/day/14

open System
open System.IO
open System.Text.RegularExpressions

let input = "aoc-14-input.txt" |> File.ReadAllLines

let input1 =
    [| "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
       "mem[8] = 11"
       "mem[7] = 101"
       "mem[8] = 0" |]

type Command =
    | Mask of char []
    | Mem of int * int

let memRegex = Regex("^mem\[(\d+)\] = (\d+)$")

let parseCommand (s: string): Command =
    if s.Substring(0, 4) = "mask" then
        let mask = s.Substring(7).ToCharArray()
        Mask mask
    else
        let groups = memRegex.Match(s).Groups
        let memSlot = groups.[1].Value |> int
        let value = groups.[2].Value |> int
        Mem(memSlot, value)

let applyMask mask (value: int) =
    Convert.ToString(value, 2).PadLeft(36, '0').ToCharArray()
    |> Array.zip mask
    |> Array.map (fun (m, b) -> if m = 'X' then b else m)
    |> String
    |> fun s -> Convert.ToInt64(s.ToString(), 2)

let processCommand ((values, currentMask): Map<int, int64> * char []) (command: Command) =
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
