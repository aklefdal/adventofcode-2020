// https://adventofcode.com/2020/day/6

let newLine = "\r\n"

let replace (old: string) (n: string) (s: string) = s.Replace(old, n)

let putAllInOneLine = replace newLine ""

let splitOnString (delimiter: string) (s: string) =
    s.Split([| delimiter |], System.StringSplitOptions.RemoveEmptyEntries)

let groupDelimiter = newLine + newLine

let input =
    "aoc-06-input.txt" |> System.IO.File.ReadAllText

let solution1 =
    input
    |> splitOnString groupDelimiter
    |> Seq.map (putAllInOneLine >> Seq.groupBy id >> Seq.length)
    |> Seq.sum

// Part 2
let countForAll (group: string) =
    let peoplesAnswers = group |> splitOnString newLine
    let countOfPeopleInGroup = peoplesAnswers.Length
    group
    |> putAllInOneLine
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.filter ((=) countOfPeopleInGroup)
    |> Seq.length

let solution2 =
    input
    |> splitOnString groupDelimiter
    |> Seq.map countForAll
    |> Seq.sum
