#load "Common.fsx"
#load "../data/day0.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Packet =
    | Int of int
    | List of Packet list

let parsePacket (line: string) =
    let rec inner soFar (pending: string option) remaining =
        if remaining = "" then
            soFar, ""
        else
            match remaining[0] with
            | ']'
            | ',' ->
                match pending with
                | None -> (List soFar), remaining[1..]
                | Some i -> soFar :: Int(int i), remaining[1..]
            | '[' ->
                let innerList, remaining = inner [] None remaining[1..]
                soFar :: innerList, remaining
            | d ->
                inner
                    soFar
                    (pending
                     |> Option.map (fun existing -> existing + string d)
                     |> Option.defaultValue (string d))
                    remaining[1..]

    inner (List []) None line


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
