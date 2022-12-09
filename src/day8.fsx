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

let isVisible (x, y) grid =
    let xs = grid |> Map.keys |> Seq.map fst
    let xMin = xs |> Seq.min
    let xMax = xs |> Seq.max
    let ys = grid |> Map.keys |> Seq.map snd
    let yMin = ys |> Seq.min
    let yMax = ys |> Seq.max

    let toTheLeft = posLeftUntil (x - 1, y) xMin |> gridValues grid |> Seq.tryMax
    let toTheRight = posRightUntil (x + 1, y) xMax |> gridValues grid |> Seq.tryMax
    let upwards = posUpUntil (x, y - 1) yMin |> gridValues grid |> Seq.tryMax
    let downwards = posDownUntil (x, y + 1) yMax |> gridValues grid |> Seq.tryMax

    let maxes = [| toTheLeft; toTheRight; upwards; downwards |]
    let ownValue = grid |> Map.find (x, y)

    maxes |> Seq.exists Option.isNone
    || maxes |> Seq.exists (fun max -> max |> Option.get < ownValue)

let part1 input =
    let grid = parse input

    let isVisible (x, y) = isVisible (x, y) grid
    grid |> Map.keys |> Seq.filter isVisible |> Seq.length

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day8.data
              Expect.equal subject 1672 ""
          }

          test "part 2" {
              let subject = part1 Day8.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
