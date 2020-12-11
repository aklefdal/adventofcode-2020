// https://adventofcode.com/2020/day/11

let input =
    "aoc-11-input.txt"
    |> System.IO.File.ReadAllLines

type State =
    | Available
    | Occupied
    | Floor

//let testInput =
//    [| "L.LL.LL.LL"
//       "LLLLLLL.LL"
//       "L.L.L..L.."
//       "LLLL.LL.LL"
//       "L.LL.LL.LL"
//       "L.LLLLL.LL"
//       "..L.L....."
//       "LLLLLLLLLL"
//       "L.LLLLLL.L"
//       "L.LLLLL.LL" |]
let toState c =
    match c with
    | 'L' -> Available
    | '#' -> Occupied
    | '.' -> Floor
    | _ -> failwith "ouch"

//let fromState s =
//    match s with
//    | Available -> 'L'
//    | Occupied  -> '#'
//    | Floor     -> '.'
//
let startFloor =
    input
    |> Array.map (fun line -> line.ToCharArray() |> Array.map toState)

//let stateToChars (floor: State[][]) : char[][] =
//    floor |> Array.map (Array.map fromState)
//
let charsToState (floor: char[][]) : State[][] =
    floor |> Array.map (Array.map toState)

let height = input.Length
let width = input.[0].Length

let getValue (floor: State[][]) ((x, y): int * int) =
    floor.[y].[x]

let getCountOfOccupiedNearbySeats (floor: State[][]) ((x, y): int * int) =
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
    |> List.filter ((=) Occupied )
    |> List.length
    
let getNewState state countOfOccupiedNeighbours =
    match state, countOfOccupiedNeighbours with
    | Available, 0 -> Occupied
    | Occupied, n when n >= 4 -> Available
    | s, _ -> s

let getNewFloor (floor: State[][]) =
    floor
    |> Array.mapi (fun y -> Array.mapi (fun x c ->
            let pos = x, y
            let count = pos |> getCountOfOccupiedNearbySeats floor
            getNewState c count))

let countOccupied = Array.sumBy (Array.filter ((=) Occupied) >> Array.length)

let rec solve floor =
    let newFloor = floor |> getNewFloor
    if newFloor = floor then
        newFloor |> countOccupied
    else
        solve newFloor
        
let solution1 = startFloor |> solve
