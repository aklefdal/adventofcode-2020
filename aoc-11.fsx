// https://adventofcode.com/2020/day/11

let input =
    "aoc-11-input.txt" |> System.IO.File.ReadAllLines

let startFloor =
    input
    |> Array.map (fun line -> line.ToCharArray())

let height = input.Length
let width = input.[0].Length

let getValue (floor: char [] []) (x, y) = floor.[y].[x]

let getCountOfOccupiedNearbySeats (floor: char [] []) ((x, y): int * int) =
    [ x - 1, y - 1
      x - 1, y
      x - 1, y + 1
      x, y - 1
      x, y + 1
      x + 1, y - 1
      x + 1, y
      x + 1, y + 1 ]
    |> List.filter (fun (a, b) -> a >= 0 && a < width && b >= 0 && b < height)
    |> List.map (getValue floor)
    |> List.filter ((=) '#')
    |> List.length

let getNewState state countOfOccupiedNeighbours =
    match state, countOfOccupiedNeighbours with
    | 'L', 0 -> '#'
    | '#', n when n >= 4 -> 'L'
    | s, _ -> s

let getNewFloor (floor: char [] []) =
    floor
    |> Array.mapi (fun y ->
        Array.mapi (fun x c ->
            let pos = x, y

            let count =
                pos |> getCountOfOccupiedNearbySeats floor

            getNewState c count))

let countOccupied =
    Array.sumBy (Array.filter ((=) '#') >> Array.length)

let rec solve floor =
    let newFloor = floor |> getNewFloor
    if newFloor = floor then newFloor |> countOccupied else solve newFloor

let solution1 = startFloor |> solve

// part 2

let (++) (dx, dy) (x, y) = x + dx, y + dy

let isOutside (a, b): bool =
    a < 0 || a >= width || b < 0 || b >= height

(1, 1) |> isOutside

let rec see (floor: char [] []) (pos: int * int) (direction: int * int): char =
    let posToCheck = pos ++ direction
    if posToCheck |> isOutside then
        '.'
    else
        match posToCheck |> getValue floor with
        | '#' -> '#'
        | 'L' -> 'L'
        | _ -> see floor posToCheck direction

let getCountOfOccupiedSeenSeats (floor: char [] []) ((x, y): int * int) =
    [ -1, -1
      -1, 0
      -1, 1
      0, -1
      0, 1
      1, -1
      1, 0
      1, 1 ]
    |> List.map (see floor (x, y))
    |> List.filter ((=) '#')
    |> List.length

let getNewState2 state countOfOccupiedNeighbours =
    match state, countOfOccupiedNeighbours with
    | 'L', 0 -> '#'
    | '#', n when n >= 5 -> 'L'
    | s, _ -> s

let rec solve2 floor =
    let newFloor =
        floor
        |> Array.mapi (fun y ->
            Array.mapi (fun x c ->
                let pos = x, y
                let count = pos |> getCountOfOccupiedSeenSeats floor
                getNewState2 c count))

    if newFloor = floor then newFloor |> countOccupied else solve2 newFloor

#time "on"
let solution2 = startFloor |> solve2
#time "off"
