// https://adventofcode.com/2020/day/11

let input =
    "aoc-11-input.txt" |> System.IO.File.ReadAllLines

type State = 
    | Available
    | Occupied
    | Floor

let toState c =
    match c with
    | 'L' -> Available
    | '#' -> Occupied
    | '.' -> Floor
    | _ -> failwith "ouch"
    
let startFloor =
    input
    |> Array.map (fun line -> line.ToCharArray() |> Array.map toState)
    
let countOccupied =  Array.sumBy (Array.filter ((=) Occupied) >> Array.length)

let height = input.Length
let width = input.[0].Length
let isOutside (x, y) =
    x < 0 || x >= width || y < 0 || y >= height

let getValueFrom (floor: State [] []) (x, y) = floor.[y].[x]

let (++) (dx, dy) (x, y) = x + dx, y + dy

let directions =
    [ -1, -1
      -1, 0
      -1, 1
      0, -1
      0, 1
      1, -1
      1, 0
      1, 1 ]

// Part 1

let seeNearestNeighbour floor pos direction =
    let posToCheck = pos ++ direction
    if posToCheck |> isOutside then
        Floor
    else
        posToCheck |> getValueFrom floor
    
let getCountOfOccupiedNearbySeats floor (x, y) =
    directions
    |> List.map (seeNearestNeighbour floor (x, y))
    |> List.filter ((=) Occupied)
    |> List.length

let getNewState state countOfOccupiedNeighbours =
    match state, countOfOccupiedNeighbours with
    | Available, 0 -> Occupied
    | Occupied, n when n >= 4 -> Available
    | s, _ -> s

let getNewFloor floor =
    floor
    |> Array.mapi (fun y ->
        Array.mapi (fun x c ->
            let pos = x, y
            let count = pos |> getCountOfOccupiedNearbySeats floor
            getNewState c count))

let rec solve floor =
    let newFloor = floor |> getNewFloor
    if newFloor = floor then
        newFloor |> countOccupied
    else
        solve newFloor

let solution1 = startFloor |> solve

// part 2

let rec see floor pos direction =
    let posToCheck = pos ++ direction
    if posToCheck |> isOutside then
        Floor
    else
        match posToCheck |> getValueFrom floor with
        | Occupied -> Occupied
        | Available -> Available
        | _ -> see floor posToCheck direction

let getCountOfOccupiedSeenSeats floor (x, y) =
    directions
    |> List.map (see floor (x, y))
    |> List.filter ((=) Occupied)
    |> List.length

let getNewState2 state countOfOccupiedNeighbours =
    match state, countOfOccupiedNeighbours with
    | Available, 0 -> Occupied
    | Occupied, n when n >= 5 -> Available
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

let solution2 = startFloor |> solve2
