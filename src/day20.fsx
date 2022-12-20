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

let insertAtSpaces l num =
    let index = l |> List.findIndex ((=) num)
    let length = l |> List.length
    let newIndex = index + (num % (length - 1))

    if newIndex >= length then
        let untilEnd = (length - 1) - index
        let newIndex = (num - untilEnd) % (length - 1)
        l |> List.removeAt index |> List.insertAt (newIndex) num
    else if newIndex < 0 then
        l |> List.removeAt index |> List.insertAt ((newIndex - 1 + length) % length) num
    else
        l |> List.removeAt index |> List.insertAt newIndex num

let normalize l =
    let rec inner l pending =
        match l with
        | [] -> failwith "expected to find 0"
        | h :: _ when h = 0 -> List.append l (List.rev pending)
        | h :: rest -> inner rest (h :: pending)

    inner l []

let tee = id

let mix originalNums =
    let afterOne =
        originalNums
        |> List.fold
            (fun acc num ->
                let index = tee ((acc) |> List.findIndex ((=) (tee num)))

                (insertAtSpaces acc num))
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
          test "around times" {
              let subject = (parse >> Seq.toList >> mix) "1\n2\n10\n-3\n3\n-2\n-11\n0\n4"
              Expect.equal subject [ 1; -11; -2; 2; 4; 10; 0; 3; -3 ] ""
          }
          test "manageable multiple times" {
              let subject = (parse >> Seq.toList >> mix >> normalize) "20\n-22\n4\n0\n5"
              Expect.equal subject [ 0; -22; 20; 5; 4 ] ""
          }
          test "manageable multiple times 2" {
              let subject = (parse >> Seq.toList >> mix >> normalize) "21\n-22\n-4\n0\n5"
              Expect.equal subject [ 0; 21; 5; -4; -22 ] ""
          }

          test "part 1" {
              let subject = part1 Day20.data
              Expect.equal subject 1 ""
              Expect.notEqual subject -20632 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
