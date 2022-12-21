#load "Common.fsx"
#load "../data/day21.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Monkey =
    | Plain of int64
    | Composite of char * string * string

let plainRegex = "(.*): (\d+)"
let compositeRegex = "(.*): (.+) (.) (.+)"

let parsePlain = extractValues plainRegex
let parseComposite = extractValues compositeRegex

let parse input =
    let lines = input |> lines

    let lineToMonkey line =
        match line |> parsePlain with
        | None ->
            let parsed = line |> parseComposite |> Option.get |> Seq.toArray
            (parsed[0], Composite(parsed[2][0], parsed[1], parsed[3]))
        | Some result ->
            let result = result |> Seq.toArray
            (result[0], Plain(int64 result[1]))

    Seq.map (lineToMonkey) lines |> Map.ofSeq


let part1 input =
    let monkeys = parse input

    let rec eval results monkey =
        match results |> Map.tryFind monkey with
        | Some result -> results, result
        | None ->
            let results, result =
                match monkeys[monkey] with
                | Plain d -> results, d
                | Composite(op, child1, child2) ->
                    let results, child1Result = eval results child1
                    let results, child2Result = eval results child2

                    let result =
                        match op with
                        | '+' -> child1Result + child2Result
                        | '-' -> child1Result - child2Result
                        | '*' -> child1Result * child2Result
                        | '/' -> child1Result / child2Result
                        | c -> failwithf "unknown op %A" c

                    results, result

            (results |> Map.add monkey result, result)

    eval (Map []) "root" |> snd

let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day21.sample
              Expect.equal subject 152 ""
          }
          test "part 1" {
              let subject = part1 Day21.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
