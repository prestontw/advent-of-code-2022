#load "Common.fsx"
#load "../data/day2.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    Seq.map (spaces) lines


[<Literal>]
let Rock = "A"

[<Literal>]
let Paper = "B"

[<Literal>]
let Scissors = "C"

[<Literal>]
let URock = "X"

[<Literal>]
let UPaper = "Y"

[<Literal>]
let UScissors = "Z"

let part1 input =
    let throws = parse input

    let score [| elf; you |] =
        let your_score =
            match you with
            | URock -> 1
            | UPaper -> 2
            | UScissors -> 3

        let winning =
            match elf, you with
            | Rock, URock
            | Paper, UPaper
            | Scissors, UScissors -> 3
            | Rock, UPaper
            | Paper, UScissors
            | Scissors, URock -> 6
            | Rock, UScissors
            | Paper, URock
            | Scissors, UPaper -> 0

        winning + your_score

    Seq.sumBy score throws

[<Literal>]
let Lose = "X"

[<Literal>]
let Draw = "Y"

[<Literal>]
let Win = "Z"

let part2 input =
    let throws = parse input

    let score [| elf; you |] =
        let outcome =
            match you with
            | Lose -> 0
            | Draw -> 3
            | Win -> 6

        let your_score =
            match elf, you with
            | Rock, Lose -> 3
            | Paper, Draw -> 2
            | Scissors, Win -> 1
            | Rock, Draw -> 1
            | Paper, Win -> 3
            | Scissors, Lose -> 2
            | Rock, Win -> 2
            | Paper, Lose -> 1
            | Scissors, Draw -> 3

        your_score + outcome

    Seq.sumBy score throws


let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day2.data
              Expect.equal subject 13565 ""
          }

          test "part 2" {
              let subject = part2 Day2.data
              Expect.equal subject 12424 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
