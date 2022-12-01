#load "Common.fsx"
#load "../data/day1.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse (input: string) = input.Split '\n' |> Seq.map int

let part1 list =

    let combinations =
        seq {
            for i in list do
                for j in list do
                    if i + j = 2022 then
                        yield (i * j)
        }

    Seq.head combinations

let tests =
    testList
        "parts"
        [ test "part 1" {
              let subject = part1 Day1.data
              Expect.equal subject 793524 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
