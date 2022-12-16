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

let interpMonkey value lcm monkey =
    let newValue =
        match monkey.operation with
        | Add x -> value + x
        | Multiply m -> value * m
        | OldSquared -> value * value

    let newValue = newValue % lcm

    let newIndex =
        if (newValue % monkey.testDivisible) = 0 then
            monkey.trueIndex
        else
            monkey.falseIndex

    newValue, newIndex

let rec next monkey items otherItems lcm =
    match items with
    | [] -> otherItems
    | head :: items ->
        let newWorry, newIndex = monkey |> interpMonkey head lcm

        let newOtherItems =
            otherItems
            |> List.updateAt newIndex (List.append otherItems[newIndex] [ newWorry ])

        next monkey items newOtherItems lcm

let rec round (monkeys: Monkey[]) (items: _ list list) monkeyIndex (inspectionCount: _[]) lcm =
    if monkeyIndex >= (monkeys |> Array.length) then
        items
    else
        let inspectionAmount = items[monkeyIndex] |> List.length

        inspectionCount[monkeyIndex] <- inspectionCount[monkeyIndex] + inspectionAmount

        let items =
            next (monkeys[monkeyIndex]) (items[monkeyIndex]) (items |> List.updateAt monkeyIndex []) lcm

        round monkeys items (monkeyIndex + 1) inspectionCount lcm


let parse startingItems =
    startingItems
    |> Seq.map (fun s -> split s ", " |> Seq.map int)
    |> Seq.map Seq.toList
    |> Seq.toList

let part1 startingItems monkeys =
    let startingItems = parse startingItems
    let inspectionCount = Array.init (monkeys |> Array.length) (fun _ -> 0)
    let rounds = seq { 1..10000 }

    let lcm =
        monkeys |> Seq.map (fun m -> m.testDivisible) |> Set.ofSeq |> Seq.fold lcm 1

    let finalItems =
        rounds
        |> Seq.fold
            (fun acc roundNumber ->
                if (roundNumber - 1) % 1000 = 0 || roundNumber = 21 || roundNumber = 2 then
                    printfn "%A" inspectionCount

                round monkeys acc 0 inspectionCount lcm)
            startingItems

    (tee inspectionCount) |> ignore
    let inspectionCount = inspectionCount |> Array.sortDescending
    (inspectionCount[0] |> uint64) * (inspectionCount[1] |> uint64)

let sampleMonkeys =
    [| { operation = Multiply 19
         testDivisible = 23
         trueIndex = 2
         falseIndex = 3 }
       { operation = Add 6
         testDivisible = 19
         trueIndex = 2
         falseIndex = 0 }
       { operation = OldSquared
         testDivisible = 13
         trueIndex = 1
         falseIndex = 3 }
       { operation = Add 3
         testDivisible = 17
         trueIndex = 0
         falseIndex = 1 } |]

let sampleItems = [ "79, 98"; "54, 65, 75, 74"; "79, 60, 97"; "74" ]

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 startingItems monkeys
              Expect.equal subject 1UL ""
          }

          test "sample" {
              let subject = part1 sampleItems sampleMonkeys
              Expect.equal subject 2713310158UL ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
