#load "Common.fsx"
#load "../data/day3.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    lines


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

    lines
    |> Seq.map duplicate
    |> Seq.sumBy (fun duplicate ->
        if duplicate > 'Z' then
            int duplicate - (int 'a') + 1
        else
            int duplicate - (int 'A') + 27)

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day3.data
              Expect.equal subject 1 ""
          }

          test "part 2" {
              let subject = part1 Day3.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
