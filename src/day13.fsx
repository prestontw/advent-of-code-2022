#load "Common.fsx"
#load "../data/day0.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type SExpr =
    | Word of string
    | Sentence of SExpr list

type Packet =
    | Int of int
    | PList of Packet list

let parsePacket (line: string) =
    let rec inner (sentence: SExpr list) (pendingWord: string option) (remaining: string) =
        if remaining = "" then

            Sentence sentence, ""
        else
            let tail = remaining[1..]

            match remaining[0] with
            | '[' ->
                let innerSentence, tail = inner [] None tail
                let sentence = List.append sentence [ innerSentence ]
                inner sentence None tail
            | ']' ->
                let sentence =
                    match pendingWord with
                    | Some s -> List.append sentence [ Word s ]
                    | None -> sentence

                (Sentence sentence), tail
            | ',' ->
                inner
                    (List.append
                        sentence
                        (pendingWord |> Option.map (fun word -> [ Word word ]) |> Option.defaultValue []))
                    None
                    tail
            | c ->
                inner
                    sentence
                    (pendingWord
                     |> Option.map (fun existing -> existing + string c)
                     |> Option.orElse (Some(string c)))
                    tail

    // take off the first '[' because we start with a list already
    inner [] None line[1..]


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
