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

let part1 input =
    let lines = parse input

    let folder (grid: Map<_, _>) (sensor, beacon) =
        let distance = manhattanPoints sensor beacon
        let (sensorX, sensorY) = sensor

        let positions =
            seq {
                for x in (sensorX - distance) .. (sensorX + distance) do
                    for y in (sensorY - distance) .. (sensorY + distance) do
                        if manhattanPoints (x, y) sensor < distance then
                            yield (x, y)
            }

        let grid =
            positions
            |> Seq.fold
                (fun grid pos ->
                    match grid |> Map.tryFind pos with
                    | None when (manhattanPoints sensor pos) < distance -> grid |> Map.add pos NotBeacon
                    | None
                    | Some _ -> grid)
                grid

        let grid = grid |> Map.add sensor Sensor
        grid |> Map.add beacon Beacon

    let grid = lines |> Seq.fold folder (Map [])

    grid
    |> Map.toSeq
    |> Seq.filter (fun ((_, y), cell) -> y = p1YCoord && cell = NotBeacon)
    |> Seq.length

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
