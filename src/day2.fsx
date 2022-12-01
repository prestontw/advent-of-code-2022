#load "Common.fsx"
#load "../data/day2.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let groups = blankLines input
    Seq.map (Seq.map double) groups |> Seq.map Seq.sum


let part1 input =
    let calories = parse input
    Seq.max calories


let part2 input =
    let calories = parse input

    let sorted = Seq.sort calories |> Seq.rev

    Seq.take 3 sorted |> Seq.sum


let tests =
    testList
        "parts"
        [ test "part 1" {
              let subject = part1 Day2.data
              Expect.equal subject 69912 ""
          }



          ]

let main = runTestsWithCLIArgs [] [||] tests
