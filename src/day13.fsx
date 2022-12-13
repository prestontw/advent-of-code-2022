#load "Common.fsx"
#load "../data/day0.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Packet =
    | Int of int
    | List of Packet list

let rec parsePacket (line: string) =
    if line = "[]" then
        List []
    else if line[0] = '[' then
        parsePacket line[1 .. (line.Length - 2)]
    else
        match System.Int32.TryParse line with
        | true, i -> Int i
        | _ -> line |> commas |> Seq.map parsePacket |> Seq.toList |> List

let parse input =
    let lines = input |> blankLines
    lines


let part1 input =
    let lines = parse input

    1

let tests =
    testList
        "parts"
        [

          //   test "parse" {
          //       let output = parsePacket "[[],[1,2],3]"
          //       let expected = List([ List []; List [ Int 1; Int 2 ]; Int 3 ])
          //       Expect.equal output expected ""
          //   }
          test "part 1" {
              let subject = part1 Day0.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
