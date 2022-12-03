#load "Common.fsx"
#load "../data/day4.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines

    let parseLine line =
        let [| fElf; sElf |] = commas line
        let [| lower; upper |] = fElf.Split '-'
        let [| lower2; upper2 |] = sElf.Split '-'
        (int lower, int upper), (int lower2, int upper2)

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
