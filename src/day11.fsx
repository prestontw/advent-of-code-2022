#load "Common.fsx"
#load "../data/day11.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Operation =
    | Multiply of int
    | Add of int
    | OldSquared

type Monkey =
    { operation: Operation
      testDivisible: int
      trueIndex: int
      falseIndex: int }

let monkeys =
    [| { operation = Multiply 17
         testDivisible = 3
         trueIndex = 3
         falseIndex = 6 }
       { operation = Add 2
         testDivisible = 13
         trueIndex = 3
         falseIndex = 0 }
       { operation = Add 1
         testDivisible = 2
         trueIndex = 0
         falseIndex = 1 }
       { operation = Add 7
         testDivisible = 11
         trueIndex = 6
         falseIndex = 7 }
       { operation = OldSquared
         testDivisible = 19
         trueIndex = 2
         falseIndex = 5 }
       { operation = Add 8
         testDivisible = 17
         trueIndex = 2
         falseIndex = 1 }
       { operation = Multiply 2
         testDivisible = 5
         trueIndex = 4
         falseIndex = 7 }
       { operation = Add 6
         testDivisible = 7
         trueIndex = 4
         falseIndex = 5 } |]

let startingItems =
    [ "59, 65, 86, 56, 74, 57, 56"
      "63, 83, 50, 63, 56"
      "93, 79, 74, 55"
      "86, 61, 67, 88, 94, 69, 56, 91"
      "76, 50, 51"
      "77, 76"
      "74"
      "86, 85, 52, 86, 91, 95" ]

let inspectionCount = Array.init (monkeys |> Array.length) (fun _ -> 0)

let interpMonkey value monkey =
    let newValue =
        match monkey.operation with
        | Add x -> value + x
        | Multiply m -> value * m
        | OldSquared -> value * value

    let newValue = newValue / 3

    let newIndex =
        if (newValue % monkey.testDivisible) = 0 then
            monkey.trueIndex
        else
            monkey.falseIndex

    newValue, newIndex

let rec next monkey items otherItems =
    match items with
    | [] -> otherItems
    | head :: items ->
        let newWorry, newIndex = monkey |> interpMonkey head

        let newOtherItems =
            otherItems
            |> List.updateAt newIndex (List.append otherItems[newIndex] [ newWorry ])

        next monkey items newOtherItems

let rec round (monkeys: Monkey[]) (items: int list list) monkeyIndex =
    if monkeyIndex >= (monkeys |> Array.length) then
        items
    else
        let inspectionAmount = items[monkeyIndex] |> List.length

        inspectionCount[monkeyIndex] <- inspectionCount[monkeyIndex] + inspectionAmount

        let items =
            next (monkeys[monkeyIndex]) (items[monkeyIndex]) (items |> List.updateAt monkeyIndex [])

        round monkeys items (monkeyIndex + 1)


let parse startingItems =
    let startingItems =
        startingItems
        |> Seq.map (fun s -> s |> split ", " |> Seq.map int)
        |> Seq.map Seq.toList
        |> Seq.toList

    let rounds = seq { 1..20 }
    let finalItems = rounds |> Seq.fold (fun acc _ -> round monkeys acc 0) startingItems
    (inspectionCount |> Array.sortDescending) |> ignore
    inspectionCount[0] * inspectionCount[1]


let part1 input = parse input

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 startingItems
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
