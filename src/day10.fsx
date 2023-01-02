#load "Common.fsx"
#load "../data/day10.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Op =
    | Noop
    | Add of int

type CycleOp =
    | Noop
    | Add of int

let opToCycle =
    function
    | Op.Noop -> [ Noop ]
    | Op.Add x -> [ Noop; Add x ]

let parse input =
    let lines = input |> lines

    let lineToOp line =
        let line = line |> spaces

        if line |> Seq.length = 1 then
            Op.Noop
        else
            Op.Add(line[1] |> int)

    Seq.map (lineToOp) lines

let signalStrength cycle xVal = cycle * xVal

let part1 input =
    let ops = parse input
    let ops = ops |> Seq.collect opToCycle

    let times = Set.empty.Add(20).Add(60).Add(100).Add(140).Add(180).Add(220)

    let sum, _cycle, _x =
        ops
        |> Seq.take 222
        |> Seq.fold
            (fun (sum, cycle, x) cur ->
                let sum =
                    if times |> Set.contains cycle then
                        tee (cycle, x) |> ignore
                        sum + signalStrength x cycle
                    else
                        sum

                let x =
                    match cur with
                    | Noop -> x
                    | Add y -> x + y

                (sum, cycle + 1, x))
            (0, 1, 1)

    sum

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day10.data
              Expect.equal subject 13140 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
