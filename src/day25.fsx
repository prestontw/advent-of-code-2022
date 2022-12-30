#load "Common.fsx"
#load "../data/day25.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    lines

let snafuToNumber snafu =
    snafu
    |> Seq.rev
    |> Seq.fold
        (fun (sum, place) c ->
            sum
            + (match c with
               | '1' -> 1L
               | '2' -> 2L
               | '0' -> 0L
               | '-' -> -1L
               | '=' -> -2L)
              * place,
            place * 5L)
        (0L, 1L)
    |> fst

let numberToSnafu number =
    let rec inner acc num =
        if num = 0L then
            acc
        else
            let (remainder, times) = num % 5L, num / 5L

            let c, carry =
                match int remainder with
                | 0
                | 1
                | 2 -> (string remainder), 0L
                | 3 -> "=", 1L
                | 4 -> "-", 1L

            inner (c + acc) (times + carry)

    inner "" number

let part1 input =
    let lines = parse input
    lines |> Seq.sumBy snafuToNumber |> numberToSnafu

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day25.data
              Expect.equal subject "2=-1=0" ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
