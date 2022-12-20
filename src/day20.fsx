#load "Common.fsx"
#load "../data/day20.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    Seq.map (int) lines

let moddedIndex l i =
    let len = List.length l
    let offset = i % len
    l[offset]

let indexAfter0 l offsetFromZero =
    let zeroPos = List.findIndex ((=) 0) l
    moddedIndex l (zeroPos + offsetFromZero)

let groveCoordinates l =
    (indexAfter0 l 1000) + (indexAfter0 l 2000) + (indexAfter0 l 3000)

let tee = id

let part1 input =
    let nums = parse input
    let originalNums = nums |> Seq.toList

    let afterOne =
        originalNums
        |> List.fold
            (fun acc num ->
                let index = tee (acc |> List.findIndex ((=) (tee num)))

                let acc = acc |> List.removeAt index

                let length = acc |> List.length
                let indexToInsertAt = tee (((index + num) % (length) + (length)) % (length))

                tee (acc |> List.insertAt indexToInsertAt num))
            originalNums

    afterOne |> groveCoordinates

let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day20.sample
              Expect.equal subject 3 ""
          }
          test "part 1" {
              let subject = part1 Day20.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
