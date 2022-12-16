#load "Common.fsx"
#load "../data/day16.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    Seq.map (spaces) lines


let part1 input =
    let lines = parse input

    1

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day16.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
