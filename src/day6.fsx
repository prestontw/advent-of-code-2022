#load "Common.fsx"
#load "../data/day6.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let marker input length =
    let unique a =
        (a |> Seq.map snd |> Seq.countBy id |> Seq.length) = length

    input
    |> Seq.indexed
    |> Seq.windowed length
    |> Seq.find unique
    |> Array.last
    |> fst
    |> fun x -> x + 1

let part1 input = marker input 4

let part2 input = marker input 14

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day6.data
              Expect.equal subject 1892 ""
          }

          test "test" {
              let subject = part2 "bvwbjplbgvbhsrlpgdmjqwftvncz"
              Expect.equal subject 23 ""
          }

          test "part 2" {
              let subject = part2 Day6.data
              Expect.equal subject 2313 ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests
