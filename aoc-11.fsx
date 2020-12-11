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

let charsToState (floor: char [] []): State [] [] = floor |> Array.map (Array.map toState)

let height = input.Length
let width = input.[0].Length

let getValue (floor: State [] []) (x, y) = floor.[y].[x]

let getCountOfOccupiedNearbySeats (floor: State [] []) ((x, y): int * int) =
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
    |> List.filter ((=) Occupied)
    |> List.length

let getNewState state countOfOccupiedNeighbours =
    match state, countOfOccupiedNeighbours with
    | Available, 0 -> Occupied
    | Occupied, n when n >= 4 -> Available
    | s, _ -> s

let getNewFloor (floor: State [] []) =
    floor
    |> Array.mapi (fun y ->
        Array.mapi (fun x c ->
            let pos = x, y

            let count =
                pos |> getCountOfOccupiedNearbySeats floor

            getNewState c count))

let countOccupied =
    Array.sumBy (Array.filter ((=) Occupied) >> Array.length)

let rec solve floor =
    let newFloor = floor |> getNewFloor
    if newFloor = floor then newFloor |> countOccupied else solve newFloor

let solution1 = startFloor |> solve

// part 2

let (++) (dx, dy) (x, y) = x + dx, y + dy

let isOutside (a, b): bool =
    a < 0 || a >= width || b < 0 || b >= height

(1, 1) |> isOutside

let rec see (floor: State [] []) (pos: int * int) (direction: int * int): State =
    let posToCheck = pos ++ direction
    if posToCheck |> isOutside then
        Floor
    else
        match posToCheck |> getValue floor with
        | Occupied -> Occupied
        | Available -> Available
        | Floor -> see floor posToCheck direction

let getCountOfOccupiedSeenSeats (floor: State [] []) ((x, y): int * int) =
    [ -1, -1
      -1, 0
      -1, 1
      0, -1
      0, 1
      1, -1
      1, 0
      1, 1 ]
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
