#load "Common.fsx"
#load "../data/day8.fsx"
#r "nuget: Expecto"
#r "nuget: FSharpPlus"

open Common
open Expecto
open FSharpPlus

let parse input =
    let lines = input |> lines

    let accLine line rowNum : Map<(int * int), int> =
        line
        |> Seq.indexed
        |> Seq.fold (fun acc (i, c) -> acc |> Map.add (rowNum, i) (charToInt c)) (Map [])

    lines
    |> Seq.indexed
    |> Seq.fold (fun acc (rowNum, line) -> acc |> Map.union (accLine line rowNum)) (Map [])

let leftUntil start endInclusive = [ endInclusive..start ] |> Seq.rev

let posLeftUntil (x, y) until =
    leftUntil x until |> Seq.map (fun x -> x, y)

let rightUntil start endInclusive = [ start..endInclusive ]

let posRightUntil (x, y) until =
    rightUntil x until |> Seq.map (fun x -> x, y)

let upUntil start endInclusive = [ endInclusive..start ] |> Seq.rev

let posUpUntil (x, y) until =
    upUntil y until |> Seq.map (fun y -> x, y)

let downUntil start endInclusive = [ start..endInclusive ]

let posDownUntil (x, y) until =
    downUntil y until |> Seq.map (fun y -> x, y)

let gridValues grid positions =
    positions |> Seq.choose (fun pos -> grid |> Map.tryFind pos)

let incrementIfSome sq ownHeight num =
    let blocked =
        Seq.tryMax sq |> Option.filter (fun max -> max >= ownHeight) |> Option.isSome

    if not blocked then num else num + 1UL

let visibleScore (x, y) grid =
    let xs = grid |> Map.keys |> Seq.map fst
    let xMin = xs |> Seq.min
    let xMax = xs |> Seq.max
    let ys = grid |> Map.keys |> Seq.map snd
    let yMin = ys |> Seq.min
    let yMax = ys |> Seq.max

    let ownValue = grid |> Map.find (x, y)

    let toTheLeft = posLeftUntil (x - 1, y) xMin |> gridValues grid

    let toTheLeft =
        toTheLeft
        |> Seq.takeWhile (fun height -> height < ownValue)
        |> Seq.length
        |> uint64
        |> incrementIfSome toTheLeft ownValue

    let toTheRight = posRightUntil (x + 1, y) xMax |> gridValues grid

    let toTheRight =
        toTheRight
        |> Seq.takeWhile (fun height -> height < ownValue)
        |> Seq.length
        |> uint64
        |> incrementIfSome toTheRight ownValue

    let upwards = posUpUntil (x, y - 1) yMin |> gridValues grid

    let upwards =
        upwards
        |> Seq.takeWhile (fun height -> height < ownValue)
        |> Seq.length
        |> uint64
        |> incrementIfSome upwards ownValue

    let downwards = posDownUntil (x, y + 1) yMax |> gridValues grid

    let downwards =
        downwards
        |> Seq.takeWhile (fun height -> height < ownValue)
        |> Seq.length
        |> uint64
        |> incrementIfSome downwards ownValue

    toTheLeft * toTheRight * upwards * downwards

let part1 input =
    let grid = parse input

    let isVisible (x, y) = visibleScore (x, y) grid
    grid |> Map.keys |> Seq.map isVisible |> Seq.max

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day8.data
              Expect.equal subject (uint64 1672) ""
          }

          test "sample" {
              let subject =
                  part1
                      "30373
25512
65332
33549
35390"

              Expect.equal subject (uint64 8) ""
          }

          test "part 2" {
              let subject = part1 Day8.data
              Expect.equal subject (uint64 1) ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
