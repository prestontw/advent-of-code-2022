#load "Common.fsx"
#load "../data/day4.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let line = "(\d+)-(\d+),(\d+)-(\d+)"
let matchLine = Common.extractValues line

let parse input =
    let lines = input |> lines

    let parseLine line =
        line
        |> matchLine
        |> Option.get
        |> Seq.map int
        |> Seq.toArray
        |> (fun [| l1; u1; l2; u2 |] -> (l1, u1), (l2, u2))

    Seq.map (parseLine) lines


let part1 input =
    let lines = parse input

    let contains ((l1, r1), (l2, r2)) =
        let contains l1 r1 l2 r2 = l2 >= l1 && r2 <= r1
        contains l1 r1 l2 r2 || contains l2 r2 l1 r1

    lines |> Seq.filter contains |> Seq.length

let part2 input =
    let lines = parse input

    let overlaps ((l1, r1), (l2, r2)) =
        let overlaps l1 r1 l2 r2 =
            r1 >= l2 && r1 <= r2 || l1 >= l2 && l1 <= r2

        overlaps l1 r1 l2 r2 || overlaps l2 r2 l1 r1

    lines |> Seq.filter overlaps |> Seq.length

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day4.data
              Expect.equal subject 413 ""
          }

          test "part 2" {
              let subject = part2 Day4.data
              Expect.equal subject 806 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
