// https://adventofcode.com/2020/day/12

let input =
    "aoc-12-input.txt"
    |> System.IO.File.ReadAllLines

let getInstruction (s: string) =
    let direction = s.Substring(0, 1).[0]
    let units = s.Substring(1) |> int
    (direction, units)

let instructions = input |> Array.map getInstruction

type ShipStatus =
    { Facing: int // degrees with North as 0, East as 90, South as 180, West as 270
      East: int
      North: int }

let normalize u =
    if u >= 360 then u - 360
    elif u < 0 then u + 360
    else u

let processInstruction (status: ShipStatus) ((d, u): char * int) : ShipStatus =
    match d with
    | 'E' -> { status with East = status.East + u }
    | 'W' -> { status with East = status.East - u }
    | 'N' -> { status with North = status.North + u }
    | 'S' -> { status with North = status.North - u }
    | 'L' -> { status with Facing = status.Facing - u |> normalize }
    | 'R' -> { status with Facing = status.Facing + u  |> normalize }
    | 'F' ->
        match status.Facing with
        | 0 -> { status with North = status.North + u }
        | 90 -> { status with East = status.East + u }
        | 180 -> { status with North = status.North - u }
        | 270 -> { status with East = status.East - u }
        | _ -> failwith "ouch"
    | _ -> failwith "ouch"
    
let startStatus = { Facing = 90; East = 0; North = 0 }
let manhattanDistance (s: ShipStatus) = System.Math.Abs(s.East) + System.Math.Abs(s.North) 
let solution1 = instructions |> Array.fold processInstruction startStatus |> manhattanDistance

// Part 2

type ShipStatus2 =
    { Ship: int * int
      WayPoint: int * int }

let turn90Left (de, dn) = -dn, de 
let turn90Right (de, dn) = dn, -de 
let turn180 (de, dn) = -de, -dn 

let (++) (x, y) (dx, dy) = x + dx, y + dy
let (--) (x2, y2) (x1, y1) = x2 - x1, y2 - y1
let multiply (m: int) (x, y) = m * x, m * y

let processInstruction2 (status: ShipStatus2) ((d, u): char * int) : ShipStatus2 =
    match d with
    | 'E' -> { status with WayPoint = status.WayPoint ++ (u, 0) }
    | 'W' -> { status with WayPoint = status.WayPoint ++ (-u, 0) }
    | 'N' -> { status with WayPoint = status.WayPoint ++ (0, u) }
    | 'S' -> { status with WayPoint = status.WayPoint ++ (0, -u) }
    | 'L'
    | 'R' ->
        match d, u with
        | 'L', 90
        | 'R', 270 -> { status with WayPoint = (status.WayPoint |> turn90Left) }
        | 'L', 180
        | 'R', 180 -> { status with WayPoint = (status.WayPoint |> turn180) }
        | 'L', 270
        | 'R', 90 -> { status with WayPoint = (status.WayPoint |> turn90Right) }
        |_ -> failwith "ouch!!"
    | 'F' -> { status with Ship = status.Ship ++ (status.WayPoint |> multiply u ) }
    | _ -> failwith "ouch"
    
let startStatus2 = { Ship = (0, 0); WayPoint = (10, 1) }

let manhattanDistance2 (s: ShipStatus2) =
    let (e, n) = s.Ship
    System.Math.Abs(e) + System.Math.Abs(n)

let solution2 = instructions |> Array.fold processInstruction2 startStatus2 |> manhattanDistance2
