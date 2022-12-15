#load "Common.fsx"
#load "../data/day15.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let lineRegex =
    "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"

let extractLineValues = extractValues lineRegex

let parse input =
    let lines = input |> lines

    Seq.map
        (fun line ->
            line
            |> extractLineValues
            |> Option.get
            |> Seq.map int
            |> Seq.toArray
            |> fun [| sensorX; sensorY; beaconX; beaconY |] -> (sensorX, sensorY), (beaconX, beaconY))
        lines

let p1YCoord = 2000000

let part1 input =
    let lines = parse input

    1

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day15.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
