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
            falling |> Seq.fold (fun acc rock -> acc |> Set.add rock) stationary, (fanIndex + 1)
        else
            processFalling falling stationary (fanIndex + 1)

    processFalling falling stationary fanIndex

let part1 input =
    let lines = parse input |> Seq.toList

    let shapesLen = shapes |> List.length

    let (corridor, _fanIndex) =
        seq { 0 .. (2022 - 1) }
        |> Seq.fold
            (fun (stationary, fanIndex) shapeIndex ->
                let maxHeight =
                    if stationary |> Set.count = 0 then
                        0
                    else
                        stationary |> Set.toSeq |> Seq.map snd |> Seq.max

                let shape = shapes[shapeIndex % shapesLen]maxHeight
                simulate lines fanIndex shape stationary)
            ((Set []), 0)

    corridor |> Set.toSeq |> Seq.map snd |> Seq.max

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day17.sample
              Expect.equal subject 3068 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
