#load "Common.fsx"
#load "../data/day17.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type VentDirection =
    | Left
    | Right

let parse input =
    let lines =
        input
        |> Seq.map (function
            | '<' -> Left
            | '>' -> Right)

    lines

let hoBar yOffset =
    [ 2; 3; 4; 5 ] |> List.map (fun x -> x, yOffset)

let cross yOffset =
    [ 3, yOffset + 2; 2, yOffset + 1; 3, yOffset + 1; 4, yOffset + 1; 3, yOffset ]

let lBar yOffset =
    [ 2, yOffset; 3, yOffset; 4, yOffset; 4, yOffset + 1; 4, yOffset + 2 ]

let vertBar yOffset =
    [ yOffset; yOffset + 1; yOffset + 2; yOffset + 3 ] |> List.map (fun y -> 2, y)

let square yOffset =
    [ 2, yOffset; 3, yOffset; 2, yOffset + 1; 3, yOffset + 1 ]

let shapes = [ hoBar; cross; lBar; vertBar; square ]

let push fallingRocks stationaryRocks direction =
    let dx =
        match direction with
        | Left -> -1
        | Right -> 1

    let wouldHitWall =
        match direction with
        | Left -> ((=) 0)
        | Right -> ((=) 6)

    let wouldHitStationary =
        let fallingRocksYs = fallingRocks |> Seq.map snd |> Set.ofSeq

        let pertinentStationaryRocks =
            stationaryRocks |> Set.filter (fun (_, y) -> fallingRocksYs |> Set.contains y)

        fallingRocks
        |> Seq.exists (fun (x, y) -> pertinentStationaryRocks |> Set.contains (x + dx, y))

    if wouldHitStationary || fallingRocks |> Seq.map fst |> Seq.exists wouldHitWall then
        fallingRocks
    else
        fallingRocks |> Seq.map (fun (x, y) -> x + dx, y)

type Falling =
    | StillFalling
    | Landed

let fall fallingRocks stationaryRocks =
    if fallingRocks |> Seq.map snd |> Seq.exists ((=) 1) then
        Landed, fallingRocks
    else
        let nextFallingRocks = fallingRocks |> Seq.map (fun (x, y) -> x, y - 1)

        if nextFallingRocks |> Seq.exists (fun pos -> stationaryRocks |> Set.contains pos) then
            Landed, fallingRocks
        else
            StillFalling, nextFallingRocks

let simulate fans fanIndex falling stationary =
    let fansLength = fans |> List.length

    let rec processFalling falling stationary fanIndex =
        let falling = push falling stationary fans[fanIndex % fansLength]
        let state, falling = fall falling stationary

        if state = Landed then
            falling |> Seq.fold (fun acc rock -> acc |> Set.add rock) stationary, (fanIndex + 1), falling
        else
            processFalling falling stationary (fanIndex + 1)

    processFalling falling stationary fanIndex

let cycleLength fans (target: uint64) =
    let shapesLength = shapes |> Seq.length
    let fansLength = fans |> Seq.length

    let rec inner states shapeIndex fanIndex stationary maxHeight =
        let shape = shapes[(shapeIndex % (shapesLength))](maxHeight + 4)
        let stationary, nextFanIndex, falling = simulate fans fanIndex shape stationary

        let maxHeight =
            let maxFell = falling |> Seq.map snd |> Seq.max

            stationary
            |> Set.toSeq
            |> Seq.filter (fun (x, y) -> y >= maxFell)
            |> Seq.map snd
            |> Seq.max

        let nextShapeIndex = shapeIndex + 1

        match states |> Map.tryFind (nextShapeIndex % shapesLength, nextFanIndex % fansLength) with
        | None ->
            let states =
                states
                |> Map.add (nextShapeIndex % shapesLength, nextFanIndex % fansLength) (1, nextShapeIndex, maxHeight)

            inner states nextShapeIndex nextFanIndex stationary maxHeight
        | Some(times, numShapes, previousHeight) when times = 3 ->
            // fast forward as close to target as possible
            let cycleLength = nextShapeIndex - numShapes
            let numberOfCycles = (target - (nextShapeIndex |> uint64)) / (cycleLength |> uint64)
            let heightDiff = maxHeight - previousHeight
            let addedHeight = numberOfCycles * (heightDiff |> uint64)

            let nextShapeIndex =
                (nextShapeIndex |> uint64) + ((cycleLength |> uint64) * numberOfCycles)

            {| Stationary = stationary
               AddedHeight = addedHeight
               ShapeIndex = nextShapeIndex
               FanIndex = nextFanIndex |}
        | Some(times, _, _) ->
            inner
                (states
                 |> Map.add
                     (nextShapeIndex % shapesLength, nextFanIndex % fansLength)
                     (times + 1, nextShapeIndex, maxHeight))
                nextShapeIndex
                nextFanIndex
                stationary
                maxHeight


    inner (Map []) 0 0 (Set []) 0

let simulateMultiple fans target =
    let shapesLen = shapes |> List.length |> uint64

    let rec inner corridor shapeIndex fanIndex maxHeight =
        if shapeIndex = target then
            corridor
        else
            let shape = shapes[(shapeIndex % shapesLen) |> int](maxHeight + 4)

            let corridor, fanIndex, falling = simulate fans fanIndex shape corridor

            let maxHeight =
                let maxFell = falling |> Seq.map snd |> Seq.max

                corridor
                |> Set.toSeq
                |> Seq.filter (fun (x, y) -> y >= maxFell)
                |> Seq.map snd
                |> Seq.max

            inner corridor (shapeIndex + 1UL) (fanIndex) maxHeight

    let fastForwarded = cycleLength fans target

    let final =
        inner
            fastForwarded.Stationary
            fastForwarded.ShapeIndex
            fastForwarded.FanIndex
            (fastForwarded.Stationary |> Set.toSeq |> Seq.maxBy snd |> snd)

    (final |> Set.toSeq |> Seq.maxBy snd |> snd |> uint64)
    + fastForwarded.AddedHeight

let part1 input target =
    let lines = parse input |> Seq.toList

    simulateMultiple lines target

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day17.sample 2022UL
              Expect.equal subject 3068UL ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
