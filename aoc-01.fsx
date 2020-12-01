// https://adventofcode.com/2020/day/1

open System.IO

// Input
let inputFilename = "aoc-01-input.txt"
let input = 
    inputFilename 
    |> File.ReadAllLines

// Part 1
let crossproduct2 elements =
  seq { for el1 in elements do
          for el2 in elements do
            yield el1, el2 }

let sum2Equals2020 (el1, el2) = el1 + el2 = 2020
let multiply2 (el1, el2) = el1 * el2

let solution1 =
    input
    |> Array.map int
    |> crossproduct2
    |> Seq.filter sum2Equals2020
    |> Seq.head
    |> multiply2

// Part 2
let crossproduct3 elements =
  seq { for el1 in elements do
          for el2 in elements do
            for el3 in elements do
              yield el1, el2, el3 }

let sum3Equals2020 (el1, el2, el3) = el1 + el2 + el3 = 2020
let multiply3 (el1, el2, el3) = el1 * el2 * el3

let solution2 =
    input
    |> Array.map int
    |> crossproduct3
    |> Seq.filter sum3Equals2020
    |> Seq.head
    |> multiply3

// Part 2, with some speed improvements at the cost of readability
#time "on"
let sortedEntries = 
    input
    |> Array.map int
    |> Array.sort

// The big entries are out of scope since adding them
// together would give too high sum
let min = sortedEntries.[0]
let min2 = sortedEntries.[1]
// Any entry higher than this would have to give too high sum
let maxEntry = 2020 - min - min2
let relevantEntry entry = entry <= maxEntry
let relevantEntries = sortedEntries |> Array.filter relevantEntry

let crossproduct3 elements =
  seq { for el1 in elements do
          for el2 in elements do
            for el3 in elements do
              yield el1, el2, el3 }

let sum3Equals2020 (el1, el2, el3) = el1 + el2 + el3 = 2020
let multiply3 (el1, el2, el3) = el1 * el2 * el3

let solution2b =
    relevantEntries
    |> crossproduct3
    |> Seq.filter sum3Equals2020
    |> Seq.head
    |> multiply3
#time "off"
