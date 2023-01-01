#load "Common.fsx"
#load "../data/day18.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines

    let accLine line =
        line |> commas |> Array.map int |> (fun [| x; y; z |] -> (x, y, z))

    Seq.map (accLine) lines

let neighborPositions (x, y, z) =
    let deltas = [ (-1, 0, 0); (1, 0, 0); (0, -1, 0); (0, 1, 0); (0, 0, -1); (0, 0, 1) ]
    deltas |> Seq.map (fun (dx, dy, dz) -> dx + x, dy + y, dz + z)

let neighborCount pos cells =
    pos
    |> neighborPositions
    |> Seq.filter (fun p -> cells |> Set.contains p)
    |> Seq.length

let visibleSides pos cells = 6 - (neighborCount pos cells)


let part1 input =
    let lines = parse input

    let cells = Set.ofSeq lines

    lines |> Seq.sumBy (fun pos -> visibleSides pos cells)

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day18.data
              Expect.equal subject 64 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
