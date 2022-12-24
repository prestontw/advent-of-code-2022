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

    let accLine line y =
        line
        |> Seq.indexed
        |> Seq.fold (fun acc (i, c) -> acc |> Map.add (i, y) (charToCell c)) (Map [])

    lines
    |> Seq.indexed
    |> Seq.fold (fun acc (y, line) -> acc |> Map.union (accLine line y)) (Map [])


let maxY grid =
    grid |> Map.toSeq |> Seq.map (fst >> snd) |> Seq.max

let maxX grid =
    grid |> Map.toSeq |> Seq.map (fst >> fst) |> Seq.max

let isRaging blizzard =
    let sum = blizzard.Upwards + blizzard.Downwards + blizzard.Left + blizzard.Right
    sum <> 0

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

let simulate grid maxX maxY =
    let starting =
        grid
        |> Map.toSeq
        |> Seq.map (fun (pos, cell) ->
            match cell with
            | Wall -> (pos, Wall)
            | Expedition -> (pos, Expedition)
            | Sink -> (pos, Sink)
            | Blizzard _ -> (pos, Blizzard emptyBlizzard))
        |> Map.ofSeq


    grid
    |> Map.toSeq
    |> Seq.fold
        (fun (acc: Map<(_ * _), _>) (pos, cell) ->

            match cell with
            | Blizzard { Upwards = u
                         Downwards = d
                         Left = l
                         Right = r } ->
                let neighbors = wrappedWalledNeighbors pos (acc, maxX, maxY)


                let acc =
                    let (k1, k2, receiver) = neighbors.Left

                    match receiver with
                    | Blizzard receiver -> acc |> Map.add (k1, k2) (Blizzard { receiver with Left = l + receiver.Left })
                    | _ -> acc

                let acc =
                    let (k1, k2, receiver) = neighbors.Right

                    match receiver with
                    | Blizzard receiver ->
                        acc |> Map.add (k1, k2) (Blizzard { receiver with Right = r + receiver.Right })
                    | _ -> acc

                let acc =
                    let (k1, k2, receiver) = neighbors.Up

                    match receiver with
                    | Blizzard receiver ->
                        acc
                        |> Map.add (k1, k2) (Blizzard { receiver with Upwards = u + receiver.Upwards })
                    | _ -> acc

                let acc =
                    let (k1, k2, receiver) = neighbors.Down

                    match receiver with
                    | Blizzard receiver ->
                        acc
                        |> Map.add (k1, k2) (Blizzard { receiver with Downwards = d + receiver.Downwards })
                    | _ -> acc

                acc
            | _ -> acc)
        starting

// keep track of different boards
let aStar (grid, maxWidth, maxHeight) (starts: seq<(int * int)>) dest =
    let openSet = System.Collections.Generic.PriorityQueue()
    starts |> Seq.iter (fun t -> openSet.Enqueue((0, t, grid), 0))
    let costs = Map(starts |> Seq.map (fun pos -> (0, pos), 0))

    let estimate (x, y) time =
        let destX, destY = dest
        (absDiff x destX) + (absDiff y destY) + time

    let rec next (gScore: Map<(int * (int * int)), int>) =
        let folder time prevPos nowPos cost nextBoard acc =
            let newScore = (acc |> Map.find (time, prevPos)) + cost
            let potential = acc |> Map.tryFind (time + 1, nowPos)
            let estimate = estimate nowPos time

            match potential with
            | Some c when c > newScore ->
                let ret = acc |> Map.add ((time + 1), nowPos) newScore
                openSet.Enqueue((time + 1, nowPos, nextBoard), newScore + estimate)
                ret
            | Some _ -> acc
            | None ->
                let ret = acc |> Map.add (time + 1, nowPos) newScore
                openSet.Enqueue((time + 1, nowPos, nextBoard), newScore + estimate)
                ret

        if (openSet.Count) = 0 then
            None
        else
            let (time, pos, board) = openSet.Dequeue()
            printf "%A %A\n" time pos
            let nextBoard = simulate board maxWidth maxHeight

            let reachable p =
                nextBoard
                |> Map.tryFind p
                |> fun cell ->
                    match cell with
                    | None -> false
                    | Some(Blizzard b) when not (b |> isRaging) -> true
                    | Some(Blizzard _)
                    | Some Wall -> false
                    | _ -> true

            if pos = dest then
                gScore |> Map.tryFind (time, pos)
            else
                let nexts = pos |> cardinalNeighbors |> List.append [ pos ] |> Seq.filter reachable

                next (nexts |> Seq.fold (fun acc cur -> folder time pos cur 1 nextBoard acc) gScore)

    next costs



let part1 input =
    let lines = parse input

    let maxX = lines |> maxX
    let maxY = lines |> maxY

    let start = lines |> Map.toSeq |> Seq.find (fun s -> snd s = Expedition) |> fst
    let dest = lines |> Map.toSeq |> Seq.find (fun s -> snd s = Sink) |> fst

    aStar (lines, maxX, maxY) [ start ] dest

let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day24.sample
              Expect.equal subject (Some 18) ""
          }
          //   test "part 1" {
          //       let subject = part1 Day24.data
          //       Expect.equal subject None ""
          //   }

          ]

let main = runTestsWithCLIArgs [] [||] tests
