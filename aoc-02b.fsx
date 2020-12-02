// https://adventofcode.com/2020/day/2
// More terse solution

//
// Part 1
//
"aoc-02-input.txt"
|> System.IO.File.ReadAllLines
|> Seq.filter (fun s ->
    let a = s.Split([| '-'; ' '; ':' |], System.StringSplitOptions.RemoveEmptyEntries)
    let min = a.[0] |> int 
    let max = a.[1] |> int 
    let letter = a.[2].[0]
    let count = a.[3] |> Seq.sumBy (fun c -> if c = letter then 1 else 0)

    min <= count && count <= max)
|> Seq.length

//
// Part 2
//
"aoc-02-input.txt"
|> System.IO.File.ReadAllLines
|> Seq.filter (fun s ->
    let a = s.Split([| '-'; ' '; ':' |], System.StringSplitOptions.RemoveEmptyEntries)
    let pos1 = a.[0] |> int 
    let pos2 = a.[1] |> int 
    let letter = a.[2].[0]
    let password = a.[3]
    let isCharAtPosition i = i <= password.Length && password.Chars(i - 1) = letter

    isCharAtPosition pos1 <> isCharAtPosition pos2)
|> Seq.length
