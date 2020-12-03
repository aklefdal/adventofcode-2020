// https://adventofcode.com/2020/day/3

open System.IO

// Input
let inputFilename = "aoc-03-input.txt"
let input = 
    inputFilename 
    |> File.ReadAllLines

let length = input.Length
let frameWidth = input.[0].Length

let isTree pos =
    let x, y = pos
    if y >= length then
        None
    else
        let x' = x % frameWidth
        input.[y].[x'] = '#' |> Some

//
// Part 1
//
let move (x, y) = x + 3, y + 1

let rec findTrees count pos =
    match pos |> isTree with
    | None -> count
    | Some isTree ->
        let newCount = if isTree then count + 1 else count
        let newPos = pos |> move
        findTrees newCount newPos

let solution1 = findTrees 0 (0, 0)

//
// Part 2
//
let movePatterns =
    [| 1, 1
       3, 1
       5, 1
       7, 1
       1, 2 |]

let move2 (movePatternX, movePatternY) (posX, posY) =
    posX + movePatternX, posY + movePatternY

let rec findTrees2 movePattern count pos =
    match pos |> isTree with
    | None -> count
    | Some isTree ->
        let newCount = if isTree then count + 1L else count
        let newPos = pos |> move2 movePattern
        findTrees2 movePattern newCount newPos

let solution1b = findTrees2 (3, 1) 0L (0, 0)

let solution2 =
    movePatterns
    |> Array.map (fun movePattern -> findTrees2 movePattern 0L (0, 0))
    |> Array.reduce (*)
