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
    |> Array.append [| 0; max + 3 |]
    |> Array.sortDescending

// This is a folder. The state is a list of node counts, where the count
// represent possible combinations from this node and out. Since we start
// with the highest node, we will always find a node's descendant in the
// state.
let countFromHighest (nodeCounts: (int * int64)  list) (node: int) =
    let countOfDescendants =
        nodes // all nodes
        |> Array.filter (fun n -> node < n && n <= node + 3 ) // descendants
        |> Array.map (fun descendant -> nodeCounts |> List.find (fun (n, c) -> n = descendant)) // counts of each descendant
        |> Array.map snd // just the count
        |> Array.fold (+) 0L// sum all the counts
        |> (fun c -> if c = 0L then 1L else c) // First entry will not have any descendants, hence the special treatment
    (node, countOfDescendants) :: nodeCounts // add this node to the state

let solution2 =
    nodes
    |> Array.fold countFromHighest []
    |> List.head
    |> snd

#time "off"
