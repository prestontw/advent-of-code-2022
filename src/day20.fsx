#load "Common.fsx"
#load "../data/day20.fsx"
#r "nuget: Expecto"

open Common
open Expecto

(*
    Initial arrangement:
811589153, 1623178306, -2434767459, 2434767459, -1623178306, 0, 3246356612

After 1 round of mixing:
0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153

After 2 rounds of mixing:
0, 2434767459, 1623178306, 3246356612, -2434767459, -1623178306, 811589153

After 3 rounds of mixing:
0, 811589153, 2434767459, 3246356612, 1623178306, -1623178306, -2434767459

After 4 rounds of mixing:
0, 1623178306, -2434767459, 811589153, 2434767459, 3246356612, -1623178306

After 5 rounds of mixing:
0, 811589153, -1623178306, 1623178306, -2434767459, 3246356612, 2434767459

After 6 rounds of mixing:
0, 811589153, -1623178306, 3246356612, -2434767459, 1623178306, 2434767459

After 7 rounds of mixing:
0, -2434767459, 2434767459, 1623178306, -1623178306, 811589153, 3246356612

After 8 rounds of mixing:
0, 1623178306, 3246356612, 811589153, -2434767459, 2434767459, -1623178306

After 9 rounds of mixing:
0, 811589153, 1623178306, -2434767459, 3246356612, 2434767459, -1623178306

After 10 rounds of mixing:
0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153
*)
let parse input =
    let lines = input |> lines

    Seq.map (int >> (fun (d: int) -> int64 d) >> (fun d -> d * 811589153L)) lines
    |> Seq.indexed

let moddedIndex l i =
    let len = List.length l
    let offset = i % len
    l[offset]

let indexAfter0 l offsetFromZero =
    let zeroPos = List.findIndex ((=) 0L) l
    moddedIndex l (zeroPos + offsetFromZero)

let groveCoordinates l =
    (indexAfter0 l 1000) + (indexAfter0 l 2000) + (indexAfter0 l 3000)

let insertAtSpaces l num =
    let index = l |> List.findIndex ((=) num)
    let originalIndex, num = num
    let length = l |> List.length
    let lengthMinus1 = (int64 length) - 1L
    let newIndex = (int64 index) + (num % lengthMinus1) |> int

    if newIndex >= length then
        let untilEnd = (lengthMinus1) - (int64 index)
        let newIndex = (num - untilEnd) % (lengthMinus1) |> int
        l |> List.removeAt index |> List.insertAt (newIndex) (originalIndex, num)
    else if newIndex < 0 then
        l
        |> List.removeAt index
        |> List.insertAt (int (((int64 newIndex - 1L) + (int64 length)) % (int64 length))) (originalIndex, num)
    else
        l |> List.removeAt index |> List.insertAt newIndex (originalIndex, num)

let normalize l =
    let rec inner l pending =
        match l with
        | [] -> failwith "expected to find 0"
        | (_, h) :: _ when h = 0L -> List.append l (List.rev pending)
        | h :: rest -> inner rest (h :: pending)

    inner l []

let tee = id

let mix originalNums =
    [ 1..10 ]
    |> Seq.fold
        (fun acc _index ->
            tee (_index - 1, acc |> normalize) |> ignore

            let afterOne =
                originalNums |> List.fold (fun acc num -> (insertAtSpaces acc num)) acc

            afterOne)
        originalNums
    |> tee


let part1 input =
    let nums = parse input
    let originalNums = nums |> Seq.toList

    originalNums |> mix |> List.map snd |> groveCoordinates

let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day20.sample
              Expect.equal subject 1623178306L ""
          }

          test "part 1" {
              let subject = part1 Day20.data
              Expect.equal subject 6871725358451L ""
              Expect.notEqual subject -20632L ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
