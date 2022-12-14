#load "Common.fsx"
#load "../data/day14.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines

    lines
    |> Seq.map (fun s ->
        split " -> " s
        |> Array.map (fun tup ->
            let [| x; y |] = tup |> commas
            int x, int y))

type Cell =
    | Sand
    | Wall

let segment (x1, y1) (x2, y2) =
    if x1 = x2 then
        let yRange = [ (min y1 y2) .. (max y1 y2) ]
        yRange |> Seq.map (fun y -> x1, y)
    else
        [ (min x1 x2) .. (max x1 x2) ] |> Seq.map (fun x -> x, y1)

let draw walls =
    let folder space [| end1; end2 |] =
        let segmentCoords = segment end1 end2
        segmentCoords |> Seq.fold (fun acc t -> Map.add t Wall acc) space

    walls
    |> Seq.fold (fun grid wall -> wall |> Seq.windowed 2 |> Seq.fold folder grid) (Map [])

let rec fall (x, y) grid bottom =
    if y >= bottom then
        true, grid
    else
        // look below
        match
            (grid |> Map.tryFind (x, y + 1)), (grid |> Map.tryFind (x - 1, y + 1)), (grid |> Map.tryFind (x + 1, y + 1))
        with
        | Some _, Some _, Some _ -> false, Map.add (x, y) Sand grid
        | None, _, _ -> fall (x, y + 1) grid bottom
        | Some _, None, _ -> fall (x - 1, y + 1) grid bottom
        | Some _, Some _, None -> fall (x + 1, y + 1) grid bottom


let part1 input =
    let grid = input |> parse |> draw

    let rec findFallForever grid count =
        let slipping, grid = fall (500, 0) grid 10000
        if slipping then count else findFallForever grid (count + 1)

    findFallForever grid 0

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day14.data
              Expect.equal subject 1 ""
          }

          test "sample" {
              let output =
                  part1
                      "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

              Expect.equal output 24 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
