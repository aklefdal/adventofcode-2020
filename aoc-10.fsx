// https://adventofcode.com/2020/day/10
#time "on"

let input =
    "aoc-10-input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map int

let max = input |> Array.max

let counts =
    input
    |> Array.append [| 0; max + 3 |]
    |> Array.sort
    |> Array.pairwise
    |> Array.map (fun (x, y) -> y - x)
    |> Array.countBy id
    |> dict

let solution1 = (counts.[1]) * (counts.[3])

// Part 2
// Start with all nodes sorted from highest to lowest
let nodes =
    input
    |> Array.toList
    |> List.append [ 0; max + 3 ]
    |> List.sortDescending

// This is a folder function to be used by e.g. List.fold. The state is a list of node counts, where the count
// represent possible combinations from this node and out. Since we start
// with the highest node, we will always find a node's descendant in the
// state.
let countFromHighest (nodeCounts: (int * int64) list) (node: int) =
    let descendants =
        nodes // all nodes
        |> List.filter (fun n -> node < n && n <= node + 3) // descendants (node number)
        |> List.map (fun descendant ->
            nodeCounts
            |> List.find (fun (n, _) -> n = descendant)) // path count of each descendant

    let countOfDescendants =
        match descendants with
        | [] -> 1L
        | desc -> desc |> List.sumBy snd

    (node, countOfDescendants) :: nodeCounts // add this node to the state

let solution2 =
    nodes
    |> List.fold countFromHighest []
    |> List.head
    |> snd

#time "off"

