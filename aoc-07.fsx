// https://adventofcode.com/2020/day/7

let splitOnString (delimiter: string) (s: string) =
    s.Split([| delimiter |], System.StringSplitOptions.RemoveEmptyEntries)

let input =
    "aoc-07-input.txt" |> System.IO.File.ReadAllLines

type Content =
    { Color: string
      Count: int }

type Rule =
    { Color: string
      Content: Content list }

let parseCount (s: string) =
    s |> System.Int32.TryParse |> snd

let parseRule (s: string) : Rule =
    let a = s |> splitOnString " bags contain "
    let contentStrings = a.[1] |> splitOnString ","
    let content =
        contentStrings
        |> Seq.map (splitOnString " ")
        |> Seq.map (fun a -> { Color = sprintf "%s %s" a.[1] a.[2]; Count = a.[0] |> parseCount })
        |> Seq.filter (fun c -> c.Count > 0)
        |> Seq.toList
    { Color = a.[0]; Content = content }

let rules =
    input
    |> Array.map parseRule

let findSuitableBags (color: string) : string[] =
    rules
    |> Array.filter (fun r ->
        r.Content |> List.exists (fun c -> c.Color = color))
    |> Array.map (fun r -> r.Color)

let rec findAllSuitableBags (colors: string[]) : string[] =
    let suitableBags =
        colors
        |> Array.collect findSuitableBags
        |> Array.append colors
        |> Array.distinct
    if suitableBags.Length = colors.Length then
        suitableBags
    else
        suitableBags |> findAllSuitableBags

let solution1 =
    "shiny gold"
    |> findSuitableBags
    |> findAllSuitableBags
    |> Array.length

// Part 2

let rec findCountContainingBags (color: string) : int =
    let bagRule = rules |> Array.find (fun r -> r.Color = color)
    let countContainingBags =
        bagRule.Content
        |> List.map (fun c -> c.Count * (c.Color |> findCountContainingBags))
        |> List.sum
    countContainingBags + 1

let solution2 =
    "shiny gold"
    |> findCountContainingBags
    |> (fun all -> all - 1)
