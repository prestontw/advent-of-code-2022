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

type Direction =
    | Left of int
    | Right of int

let amt d =
    match d with
    | Left d
    | Right d -> d

let rec insertAtSpaces l num index amount =
    if amt amount = 0 then
        l
    else
        match index, amount with
        | 0, Left d ->
            insertAtSpaces
                (List.removeAt index l |> List.insertAt ((List.length l) - 2) num)
                num
                ((List.length l) - 2)
                (Left(d - 1))
        | e, Right d when e = (l |> List.length |> (fun d -> d - 1)) ->
            insertAtSpaces (l |> List.removeAt e |> List.insertAt 0 num) num 0 (Right(d - 1))
        | _, Right d ->
            insertAtSpaces (l |> List.removeAt index |> List.insertAt (index + 1) num) num (index + 1) (Right(d - 1))
        | _, Left d ->
            insertAtSpaces (l |> List.removeAt index |> List.insertAt (index - 1) num) num (index - 1) (Left(d - 1))

let tee = id

let mix originalNums =
    let afterOne =
        originalNums
        |> List.fold
            (fun acc num ->
                let index = tee ((tee acc) |> List.findIndex ((=) (tee num)))

                tee (
                    insertAtSpaces
                        acc
                        num
                        index
                        (if num < 0 then
                             Left(abs (num % (acc |> List.length)))
                         else
                             Right(num % (acc |> List.length)))
                )

                )
            originalNums

    afterOne


let part1 input =
    let nums = parse input
    let originalNums = nums |> Seq.toList

    originalNums |> mix |> groveCoordinates

let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day20.sample
              Expect.equal subject 3 ""
          }
          //   test "around times" {
          //       let subject = (parse >> Seq.toList >> mix) "1\n2\n10\n-3\n3\n-2\n-11\n0\n4"
          //       Expect.equal subject [ -3; 1; -11; 4; -2; 2; 10; 0; 3 ] ""

          // }
          test "part 1" {
              let subject = part1 Day20.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
