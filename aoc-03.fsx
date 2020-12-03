// https://adventofcode.com/2020/day/3

let input = "aoc-03-input.txt" |> System.IO.File.ReadAllLines
let inputLength = input.Length
let inputWidth = input.[0].Length

let isTree (x, y) =
    if y >= inputLength then
        None
    else
        input.[y].[x % inputWidth] = '#' |> Some

let move (deltaX, deltaY) (posX, posY) =  posX + deltaX, posY + deltaY

let rec findTrees delta count pos =
    match pos |> isTree with
    | None -> count
    | Some isTree ->
        let newCount = if isTree then count + 1L else count
        let newPos = pos |> move delta
        findTrees delta newCount newPos

// Part 1
let solution1 = findTrees (3, 1) 0L (0, 0)

// Part 2
let solution2 =
    [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
    |> List.map (fun delta -> findTrees delta 0L (0, 0))
    |> List.reduce (*)
