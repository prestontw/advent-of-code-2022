#load "Common.fsx"
#load "../data/day18.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines

    let accLine line =
        line |> commas |> Array.map int |> (fun [| x; y; z |] -> (x, y, z))

    Seq.map (accLine) lines

let neighborPositions (x, y, z) =
    let deltas = [ (-1, 0, 0); (1, 0, 0); (0, -1, 0); (0, 1, 0); (0, 0, -1); (0, 0, 1) ]
    deltas |> Seq.map (fun (dx, dy, dz) -> dx + x, dy + y, dz + z)

let neighborCount pos cells =
    pos
    |> neighborPositions
    |> Seq.filter (fun p -> cells |> Set.contains p)
    |> Seq.length

let visibleSides pos cells = 6 - (neighborCount pos cells)

let exposedSides pos airCells = neighborCount pos airCells

type Box =
    { minx: int
      maxx: int
      miny: int
      maxy: int
      minz: int
      maxz: int }

let expand box =
    { minx = box.minx - 1
      maxx = box.maxx + 1
      minz = box.minz - 1
      maxz = box.maxz + 1
      miny = box.miny - 1
      maxy = box.maxy + 1 }

let boundingBox cells =
    let xs = cells |> Set.map (fun (x, y, z) -> x)
    let ys = cells |> Set.map (fun (x, y, z) -> y)
    let zs = cells |> Set.map (fun (x, y, z) -> z)

    { minx = xs |> Set.minElement
      maxx = xs |> Set.maxElement
      miny = ys |> Set.minElement
      maxy = ys |> Set.maxElement
      minz = zs |> Set.minElement
      maxz = zs |> Set.maxElement }

let isWithin box (x, y, z) =
    x >= box.minx
    && x <= box.maxx
    && y >= box.miny
    && y <= box.maxy
    && z >= box.minz
    && z <= box.maxz

// Return the cells outside of the parameter cells and within the box
let surroundingCells box lavaCells =
    let start = (box.minx, box.miny, box.minz)

    let rec inner queue seen =
        if queue |> Seq.isEmpty then
            seen
        else
            let pos, rest = queue |> Seq.head, queue |> Seq.tail
            let neighborPoss = neighborPositions pos

            let neighbors =
                neighborPoss
                |> Seq.filter (isWithin box)
                |> Seq.filter (fun pos -> not (seen |> Set.contains pos))
                |> Seq.filter (fun pos -> not (lavaCells |> Set.contains pos))

            inner (Seq.append rest neighbors) (neighbors |> Seq.fold (fun seen pos -> seen |> Set.add pos) seen)

    inner (seq { start }) (Set [])


let part1 input =
    let lines = parse input

    let cells = Set.ofSeq lines
    let box = boundingBox cells |> expand

    let surroundingCells = surroundingCells box cells

    lines |> Seq.sumBy (fun pos -> exposedSides pos surroundingCells)

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day18.sample
              Expect.equal subject 58 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
