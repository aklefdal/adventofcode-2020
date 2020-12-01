// https://adventofcode.com/2020/day/1

open System.IO

let inputFilename = "aoc-01-input.txt"

let crossproduct2 elements =
  seq { for el1 in elements do
          for el2 in elements do
            yield el1, el2 }

let entries = 
    inputFilename 
    |> File.ReadAllLines
    |> Array.map int

let sum2Equals2020 (el1, el2) = el1 + el2 = 2020
let multiply2 (el1, el2) = el1 * el2

entries
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

crossproduct3 entries
|> Seq.filter sum3Equals2020
|> Seq.head
|> multiply3

