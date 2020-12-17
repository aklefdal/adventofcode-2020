// https://adventofcode.com/2020/day/17

open System.IO

let input =
    "aoc-17-input.txt"
    |> File.ReadAllLines
    |> Array.map (fun line -> line.ToCharArray() |> List.ofArray)
    |> List.ofArray

let input1 =
    [| ".#."; "..#"; "###" |]
    |> Array.map (fun line -> line.ToCharArray() |> List.ofArray)
    |> List.ofArray

let (+++) (dx, dy, dz) (x, y, z) = x + dx, y + dy, z + dz

let parseLine y line =
    line
    |> List.mapi (fun x c -> if c = '#' then Some(x, y, 0) else None)
    |> List.choose id

let initialState =
    input
    |> List.mapi parseLine
    |> List.concat
    |> Set.ofList

let neighbours pos =
    seq {
        for x = -1 to 1 do
            for y = -1 to 1 do
                for z = -1 to 1 do
                    if (x, y, z) <> (0,0,0) then
                        yield (x, y, z)
    }
    |> Seq.map ((+++) pos)
    |> List.ofSeq

let isActive state pos = state |> Set.contains pos

let subtract set1 set2 = Set.difference set2 set1

let allNeighbours state =
    state
    |> Seq.map neighbours
    |> Seq.concat
    |> Set.ofSeq
    |> subtract state

let activeNeighbours state pos =
    pos |> neighbours |> List.filter (isActive state)

let isActiveStillActive state pos =
    pos
    |> activeNeighbours state
    |> Set.ofSeq
    |> Set.count
    |> (fun c -> c = 2 || c = 3)

let isInactiveToBeActive state pos =
    pos
    |> activeNeighbours state
    |> Set.ofSeq
    |> Set.count
    |> ((=) 3)


let rec finalState count state =
    let stillActive =
        state
        |> Seq.filter (isActiveStillActive state)
        |> Seq.toList

    let newActive =
        state
        |> allNeighbours
        |> Seq.filter (isInactiveToBeActive state)
        |> Seq.toList

    let newState =
        stillActive |> List.append newActive |> Set.ofList

    if count <= 1 then newState else newState |> finalState (count - 1)

let solution1 =
    initialState |> finalState 6 |> Set.count
