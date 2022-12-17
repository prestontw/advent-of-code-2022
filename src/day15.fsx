#load "Common.fsx"
#load "../data/day15.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let lineRegex =
    "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"

let extractLineValues = extractValues lineRegex

// can add each cell at border; if a cell has adjacency count >= 4, that's it!
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

let incrementOrOne value =
    value |> Option.map ((+) 1) |> Option.orElse (Some 1)


let pointsAtDistance d (pX, pY) =
    let leftToUp =
        seq {
            for x in -d .. -1 do
                let y = d + x
                yield x + pX, y + pY
        }

    let upToRight =
        seq {
            for y in d .. -1 .. 1 do
                let x = d - y
                yield x + pX, y + pY
        }

    let rightToDown =
        seq {
            for x in d .. -1 .. 1 do
                let y = x - d
                yield x + pX, y + pY
        }

    let downToLeft =
        seq {
            for y in -d .. -1 do
                let x = -d - y
                yield x + pX, y + pY
        }

    leftToUp
    |> Seq.append upToRight
    |> Seq.append rightToDown
    |> Seq.append downToLeft


let part1 input boundary =
    let lines = parse input
    // for each sensor, beacon
    let counts =
        lines
        |> Seq.fold
            (fun acc (sensor, beacon) ->
                //  get distance between
                let distance = manhattanPoints sensor beacon
                //  get points around sensor at 1 + distance
                let adjacentPoints = sensor |> pointsAtDistance (distance + 1)

                adjacentPoints
                |> Seq.fold
                    (fun acc point ->
                        let x, y = point

                        // if point in points within bounds (and optionally not covered by another sensor)
                        if x >= 0 && x <= boundary && y >= 0 && y <= boundary then
                            // add point to dictionary
                            acc |> Map.change point incrementOrOne
                        else
                            acc)
                    acc)
            (Map [])
    // find points in dictionary that has count >= 4
    let points = counts |> Map.toSeq |> Seq.filter (fun (_, count) -> count >= 4)

    let distances =
        lines
        |> Seq.map (fun (sensor, beacon) ->
            let distance = manhattanPoints sensor beacon
            sensor, distance)

    // filter these points out to those that aren't interior
    let points =
        points
        |> Seq.filter (fun (point, _count) ->
            distances
            |> Seq.forall (fun (sensor, distance) ->
                let interior = (manhattanPoints sensor point) <= distance
                not interior))

    (tee (points |> Seq.toList)) |> ignore
    let (x, y), _count = points |> Seq.item 0

    (uint64 x) * 4000000UL + (uint64 y)

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
              let subject = part1 Day15.data 4000000
              Expect.equal subject 1UL ""
          }

          test "sample" {
              let subject = part1 sample 20
              Expect.equal subject 56000011UL ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
