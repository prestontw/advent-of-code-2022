#load "Common.fsx"
#load "../data/day0.fsx"
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
              let subject = part1 Day0.data
              Expect.equal subject 1 ""
          }

          test "part 2" {
              let subject = part1 Day0.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
