#load "Common.fsx"
#load "../data/day24.fsx"
#r "nuget: Expecto"
#r "nuget: FSharpPlus"

open Common
open Expecto
open FSharpPlus

type Directions =
    | Up
    | Left
    | Down
    | Right

type BlizzardCount =
    { Upwards: int
      Downwards: int
      Left: int
      Right: int }

let emptyBlizzard =
    { Upwards = 0
      Downwards = 0
      Left = 0
      Right = 0 }

type Cell =
    | Expedition
    | Sink
    | Wall
    | Blizzard of BlizzardCount

let charToCell c =
    match c with
    | '#' -> Wall
    | 'E' -> Expedition
    | '^' -> Blizzard { emptyBlizzard with Upwards = 1 }
    | '>' -> Blizzard { emptyBlizzard with Right = 1 }
    | '<' -> Blizzard { emptyBlizzard with Left = 1 }
    | 'v' -> Blizzard { emptyBlizzard with Downwards = 1 }
    | '.' -> Blizzard emptyBlizzard
    | 'S' -> Sink

let parse input =
    let lines = input |> lines

    let accLine line rowNum =
        line
        |> Seq.indexed
        |> Seq.fold (fun acc (i, c) -> acc |> Map.add (rowNum, i) (charToCell c)) (Map [])

    lines
    |> Seq.indexed
    |> Seq.fold (fun acc (rowNum, line) -> acc |> Map.union (accLine line rowNum)) (Map [])


let gridWidth grid =
    grid |> Map.toSeq |> Seq.map (fst >> fst) |> Seq.max

let gridHeight grid =
    grid |> Map.toSeq |> Seq.map (fst >> snd) |> Seq.max

let getOrWrap direct wrap grid =
    let directCell = grid |> Map.find direct

    if directCell = Wall then
        let wrappedCell = grid |> Map.find wrap
        let (wrapX, wrapY) = wrap
        (wrapX, wrapY, wrappedCell)
    else
        let (directX, directY) = direct
        (directX, directY, directCell)

let directionOrWrap pos direction (grid, maxWidth, maxHeight) =
    let (x, y) = pos

    match direction with
    | Left -> getOrWrap (x - 1, y) (maxWidth - 1, y) grid
    | Up -> getOrWrap (x, y - 1) (x, maxHeight - 1) grid
    | Down -> getOrWrap (x, y + 1) (x, 1) grid
    | Right -> getOrWrap (x + 1, y) (1, y) grid


let wrappedWalledNeighbors pos gridInfo =
    let left = directionOrWrap pos Left gridInfo
    let right = directionOrWrap pos Right gridInfo
    let up = directionOrWrap pos Up gridInfo
    let down = directionOrWrap pos Down gridInfo

    {| Left = left
       Right = right
       Up = up
       Down = down |}

let simulate grid maxWidth maxHeight =
    let starting =
        grid
        |> Map.toSeq
        |> Seq.map (fun (pos, cell) ->
            match cell with
            | Wall -> (pos, Wall)
            | _ -> (pos, Blizzard emptyBlizzard))
        |> Map.ofSeq

    let leftCount counts =
        match counts with
        | Blizzard { Left = u } -> u
        | _ -> 0

    let rightCount counts =
        match counts with
        | Blizzard { Right = u } -> u
        | _ -> 0

    let upcount counts =
        match counts with
        | Blizzard { Upwards = u } -> u
        | _ -> 0

    let downCount counts =
        match counts with
        | Blizzard { Downwards = u } -> u
        | _ -> 0

    let unwrapBlizzard cell =
        match cell with
        | Blizzard b -> b

    grid
    |> Map.toSeq
    |> Seq.fold
        (fun (acc: Map<(_ * _), _>) (pos, cell) ->
            tee pos |> ignore

            match cell with
            | Blizzard { Upwards = u
                         Downwards = d
                         Left = l
                         Right = r } ->
                let neighbors = wrappedWalledNeighbors pos (acc, maxWidth, maxHeight)


                let acc =
                    let (k1, k2, receiver) = neighbors.Left
                    let receiver = receiver |> unwrapBlizzard
                    acc |> Map.add (k1, k2) (Blizzard { receiver with Left = l + receiver.Left })

                let acc =
                    let (k1, k2, receiver) = neighbors.Right
                    let receiver = receiver |> unwrapBlizzard
                    acc |> Map.add (k1, k2) (Blizzard { receiver with Right = r + receiver.Right })

                let acc =
                    let (k1, k2, receiver) = neighbors.Up
                    let receiver = receiver |> unwrapBlizzard

                    acc
                    |> Map.add (k1, k2) (Blizzard { receiver with Upwards = u + receiver.Upwards })

                let acc =
                    let (k1, k2, receiver) = neighbors.Down
                    let receiver = receiver |> unwrapBlizzard

                    acc
                    |> Map.add (k1, k2) (Blizzard { receiver with Downwards = d + receiver.Downwards })

                acc
            | _ -> acc)
        starting

let part1 input =
    let lines = parse input

    let maxWidth = lines |> gridWidth
    let maxHeight = lines |> gridHeight

    1

let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day24.sample
              Expect.equal subject 18 ""
          }
          test "part 1" {
              let subject = part1 Day24.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
