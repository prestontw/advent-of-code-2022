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

    let starting = Array.init (ops |> Seq.length) (fun _ -> '.')

    let display, _cycle, _x =
        ops
        |> Seq.fold
            (fun ((display: char array), cycle, x) cur ->
                let withinX = (absDiff ((cycle - 1) % 40) x) <= 1
                display[cycle - 1] <- if withinX then '#' else '.'


                let x =
                    match cur with
                    | Noop -> x
                    | Add y -> x + y

                (display, cycle + 1, x))
            (starting, 1, 1)

    display
    |> Array.chunkBySize 40
    |> Array.map (fun line -> line |> Seq.fold (fun acc c -> acc + string c) "")
    |> printfn "%A"

    0

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
