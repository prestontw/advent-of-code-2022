#load "Common.fsx"
#load "../data/day21.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Fraction = int64 * int64

type Monkey =
    | Plain of int64
    | Composite of char * string * string

type Expression =
    | Numeric of int64
    | Deferred of Fraction * Fraction
    | Equality of Expression * Expression
    | Human

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

let simplifyMult (num, den) constant =
    if den % constant = 0L then
        (num, den / constant)
    else
        (num * constant, den)

let simplifyDiv (num, den) constant =
    if num % constant = 0L then
        (num / constant, den)
    else
        (num, den * constant)

let part1 input =
    let monkeys = parse input

    let rec eval results monkey =
        match results |> Map.tryFind monkey with
        | Some result -> results, result
        | None ->
            let results, result =
                match monkeys[monkey] with
                | Plain d -> results, Numeric d
                | Composite(op, child1, child2) ->
                    let results, child1Result = eval results child1
                    let results, child2Result = eval results child2

                    if monkey = "root" then
                        (results, Equality(child1Result, child2Result))
                    else
                        let result =
                            match op, child1Result, child2Result with
                            | '+', Numeric child1Result, Numeric child2Result -> Numeric(child1Result + child2Result)
                            | '+', Numeric b, Deferred(factor, constant)
                            | '+', Deferred(factor, constant), Numeric b ->
                                let (numerator, denominator) = constant
                                Deferred(factor, (numerator + b * denominator, denominator))
                            | '-', Numeric child1Result, Numeric child2Result -> Numeric(child1Result - child2Result)
                            | '-', Human, Numeric constant -> Deferred((1, 1), (-constant, 1))
                            | '-', Numeric b, Deferred(factor, constant) ->
                                let (num, den) = constant
                                let (fnum, fden) = factor
                                Deferred((-fnum, fden), (b * den - num, den))
                            | '-', Deferred(factor, constant), Numeric b ->
                                let (num, den) = constant
                                Deferred(factor, (num - b * den, den))
                            | '-', a, b when a = b -> Numeric 0
                            | '*', Numeric child1Result, Numeric child2Result -> Numeric(child1Result * child2Result)
                            | '*', Numeric 0L, _
                            | '*', _, Numeric 0L -> Numeric 0
                            | '*', Numeric b, Deferred(factor, constant)
                            | '*', Deferred(factor, constant), Numeric b ->
                                let (fnum, fden) = simplifyMult factor b
                                let (num, den) = simplifyMult constant b
                                Deferred((fnum, fden), (num, den))
                            | '/', Numeric child1Result, Numeric child2Result -> Numeric(child1Result / child2Result)
                            | '/', Numeric 0L, _ -> Numeric 0
                            | '/', Deferred(factor, constant), Numeric b ->
                                let (fnum, fden) = simplifyDiv factor b
                                let (num, den) = simplifyDiv constant b
                                Deferred((fnum, fden), (num, den))
                            | c -> failwithf "unknown op %A" c

                        results, result

            (results |> Map.add monkey result, result)

    eval (Map [ ("humn", Human) ]) "root" |> snd

let tests =
    testList
        "parts"
        [

          // could be simplified, but *shrug*
          test "sample" {
              let subject = part1 Day21.sample
              Expect.equal subject (Equality(Deferred((2L, 4L), (-2L, 4L)), Numeric 150L)) ""
          }
          test "part 1" {
              let subject = part1 Day21.data
              Expect.equal subject (Equality(Deferred((-160L, 9L), (340883953559596L, 3L)), Numeric 55897899750372L)) ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
