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

type Cell =
    | Beacon
    | Sensor
    | NotBeacon

let part1a input yCoord =
    let lines = parse input

    let folder (grid: Map<_, _>) (sensor, beacon) =
        let distance = manhattanPoints sensor beacon
        let (sensorX, sensorY) = sensor

        let positions =
            seq {
                for x in (sensorX - distance - 1) .. (sensorX + distance + 1) do
                    for y in (sensorY - distance - 1) .. (sensorY + distance + 1) do
                        if manhattanPoints (x, y) sensor <= distance then
                            yield (x, y)
            }

        let grid =
            positions
            |> Seq.fold
                (fun grid pos ->
                    match grid |> Map.tryFind pos with
                    | None -> grid |> Map.add pos NotBeacon
                    | Some _ -> grid)
                grid

        let grid = grid |> Map.add sensor Sensor
        grid |> Map.add beacon Beacon

    let grid = lines |> Seq.fold folder (Map [])

    grid


let part1 input ycoord =
    let grid = part1a input ycoord

    let positions =
        seq {
            for x in 0..4000000 do
                for y in 0..4000000 do
                    yield (x, y)
        }

    let x, y =
        positions |> Seq.find (fun pos -> grid |> Map.tryFind pos |> Option.isNone)

    x * 4000000 + y

let sample =
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day15.data p1YCoord
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
