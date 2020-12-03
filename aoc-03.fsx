// https://adventofcode.com/2020/day/3

let input = "aoc-03-input.txt" |> System.IO.File.ReadAllLines
let inputLength = input.Length
let inputWidth = input.[0].Length

let (++) (dx, dy) (x, y) =  x + dx, y + dy

let isTree (x, y) =
    if y >= inputLength then
        None
    else
        input.[y].[x % inputWidth] = '#' |> Some

let findTrees delta =
    let rec findTreesInternal delta count pos =
        match pos |> isTree with
        | None -> count
        | Some true -> findTreesInternal delta (count + 1) (pos ++ delta)
        | Some false -> findTreesInternal delta count (pos ++ delta)
    findTreesInternal delta 0 (0, 0)

// Part 1
let solution1 = findTrees (3, 1)

// Part 2
let solution2 =
    [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
    |> List.map (findTrees >> int64)
    |> List.reduce (*)

// Using unfold, based upon https://github.com/codybartfast/advent-of-code/blob/main/Day03.fs
// by https://twitter.com/codybartfast

let getNextPosition delta pos =
    match pos ++ delta with
    | (_, newY) when newY >= inputLength -> None
    | nextPos -> Some (pos, nextPos)

let countTrees delta =
    (0, 0)
    |> Seq.unfold (getNextPosition delta)
    |> Seq.map (fun (x, y) -> input.[y].[x % inputWidth])
    |> Seq.filter ((=) '#')
    |> Seq.length
    
let solution1b = (3, 1) |> countTrees

let solution2b =
    [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
    |> List.map (countTrees >> int64)
    |> List.reduce (*)
