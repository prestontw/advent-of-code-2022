#load "Common.fsx"
#load "../data/day1.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let calories input =
    let groups = blankLines input
    Seq.map (Seq.map int) groups |> Seq.map Seq.sum


let part1 input =
    let calories = calories input
    Seq.max calories


let part2 input =
    let calories = calories input

    let sorted = Seq.sort calories |> Seq.rev

    Seq.sum (Seq.take 3 sorted)


let tests =
    testList
        "parts"
        [ test "part 1" {
              let subject = part1 Day1.data
              Expect.equal subject 69912 ""
          }

          test "part 2" {
              let subject = part2 Day1.data
              Expect.equal subject 208180 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
