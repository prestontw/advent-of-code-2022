#load "Common.fsx"
#load "../data/day12.fsx"
#r "nuget: Expecto"
#r "nuget: FSharpPlus"


open Common
open Expecto
open FSharpPlus

let parse input =
    let lines = input |> lines
    let mutable start = 0, 0
    let mutable dest = 0, 0

    let accLine line rowNum : Map<(int * int), int> =
        line
        |> Seq.indexed
        |> Seq.fold
            (fun acc (i, c) ->
                if c = 'S' then
                    start <- rowNum, i
                    acc |> Map.add (rowNum, i) 0
                else if c = 'E' then
                    dest <- rowNum, i
                    acc |> Map.add (rowNum, i) (int 'z' - (int 'a'))
                else
                    acc |> Map.add (rowNum, i) (int c - (int 'a')))
            (Map [])

    let instructions =
        lines
        |> Seq.indexed
        |> Seq.fold (fun acc (rowNum, line) -> acc |> Map.union (accLine line rowNum)) (Map [])

    (instructions, start, dest)

let aStar board starts dest =
    let openSet = System.Collections.Generic.PriorityQueue()
    starts |> Seq.iter (fun t -> openSet.Enqueue(t, 0))
    let costs = Map(starts |> Seq.map (fun pos -> pos, 0))

    let estimate (x, y) =
        let destX, destY = dest
        (absDiff x destX) + (absDiff y destY)

    let rec next (gScore: Map<(int * int), int>) =
        let folder origin acc (cost, cur) =
            let newScore = (acc |> Map.find origin) + cost
            let potential = acc |> Map.tryFind cur
            let estimate = estimate cur

            match potential with
            | Some c when c > newScore ->
                let ret = acc |> Map.add cur newScore
                openSet.Enqueue(cur, newScore + estimate)
                ret
            | Some _ -> acc
            | None ->
                let ret = acc |> Map.add cur newScore
                openSet.Enqueue(cur, newScore + estimate)
                ret

        if (openSet.Count) = 0 then
            None
        else
            let pos = openSet.Dequeue()
            let posElevation = board |> Map.find pos

            let reachable p =
                board
                |> Map.tryFind p
                |> Option.map (fun x -> x <= posElevation + 1)
                |> Option.defaultValue false

            if pos = dest then
                gScore |> Map.tryFind pos
            else
                let nexts =
                    pos |> cardinalNeighbors |> Seq.filter reachable |> Seq.map (fun pos -> 1, pos)

                next (nexts |> Seq.fold (folder pos) gScore)

    next costs

let part1 input =
    let lines, start, dest = parse input

    aStar lines [ start ] dest |> Option.get

let part2 input =
    let lines, _, dest = parse input

    let potentialOrigins =
        lines |> Map.toSeq |> Seq.filter (fun t -> (snd t) = 0) |> Seq.map fst

    aStar lines potentialOrigins dest

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day12.data
              Expect.equal subject 370 ""
          }

          test "sample" {
              let subject =
                  part1
                      "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

              Expect.equal subject 31 ""
          }

          test "part 2" {
              let subject = part2 Day12.data
              Expect.equal subject (Some 363) ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
