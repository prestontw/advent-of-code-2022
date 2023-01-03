#load "Common.fsx"
#load "../data/day3.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    lines

let scoreDuplicates duplicate items =
    items
    |> Seq.map duplicate
    |> Seq.sumBy (fun duplicate ->
        if duplicate > 'Z' then
            int duplicate - (int 'a') + 1
        else
            int duplicate - (int 'A') + 27)

let part1 input =
    let lines = parse input

    let duplicate (line: string) =
        let half = (line.Length - 1) / 2
        let first, second = line[0..half], line[half + 1 .. line.Length]

        first
        |> Set.ofSeq
        |> Set.intersect (second |> Set.ofSeq)
        |> Set.toList
        |> List.head

    lines |> scoreDuplicates duplicate

let part2 input =
    let lines = parse input

    let duplicate (lines: string[]) =
        lines
        |> Seq.collect (Set.ofSeq >> Set.toSeq)
        |> Seq.countBy id
        |> Seq.find (fun (c, count) -> count = lines.Length)
        |> fst


    lines |> Seq.chunkBySize 3 |> scoreDuplicates duplicate

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day3.data
              Expect.equal subject 8072 ""
          }

          test "part 2" {
              let subject = part2 Day3.data
              Expect.equal subject 2567 ""
          }

          ]

runTestsWithCLIArgs [] [||] tests
|> if (System.Environment.GetEnvironmentVariable("CI")) |> isNull |> not then
       exit
   else
       ignore
