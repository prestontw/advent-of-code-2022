#load "Common.fsx"
#load "../data/day22.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Cell =
    | Wall
    | Space

type Orientation =
    | North
    | South
    | West
    | East

type Directions =
    | Left
    | Right

type Instruction =
    | Move of int
    | Face of Directions

let rec intersperse l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | [], a
    | a, [] -> a
    | a :: aRest, b :: bRest -> a :: b :: (intersperse aRest bRest)

let parse input =
    let [ grid; instructions ] = blankLines input |> Seq.toList

    let accLine acc line rowNum =
        line
        |> Seq.indexed
        |> Seq.filter (fun (_i, c) -> c <> ' ')
        |> Seq.fold
            (fun acc (i, c) ->
                match c with
                | '.' -> acc |> Map.add (i, rowNum) Space
                | '#' -> acc |> Map.add (i, rowNum) Wall
                | _ -> acc)
            acc

    let grid =
        grid
        |> Seq.indexed
        |> Seq.fold (fun acc (rowNum, line) -> accLine acc line rowNum) (Map [])

    let [| instructions |] = instructions
    let nums = instructions.Split [| 'R'; 'L' |] |> Seq.map int

    let dirs =
        instructions
        |> Seq.choose (fun c ->
            match c with
            | 'R' -> Some Right
            | 'L' -> Some Left
            | _ -> None)

    grid, intersperse (nums |> Seq.map Move |> Seq.toList) (dirs |> Seq.map Face |> Seq.toList)


let turn orientation direction =
    match orientation, direction with
    | North, Left -> West
    | North, Right -> East
    | South, Left -> East
    | South, Right -> West
    | West, Left -> South
    | West, Right -> North
    | East, Left -> North
    | East, Right -> South

let start grid =
    grid
    |> Map.toSeq
    |> Seq.filter (fun ((_x, y), c) -> y = 0 && c = Space)
    |> Seq.minBy (fun ((x, _y), _c) -> x)

let getWrappedCell (x, y) dir grid =
    let filter, agg =
        match dir with
        | North -> (fun ((cX, _y), _c) -> x = cX), (fun s -> s |> Seq.maxBy (fun ((_x, y), _c) -> y))
        | South -> (fun ((cX, _y), _c) -> x = cX), (fun s -> s |> Seq.minBy (fun ((x, y), c) -> y))
        | West -> (fun ((x, cy), c) -> cy = y), (fun s -> s |> Seq.maxBy (fun ((x, y), c) -> x))
        | East -> (fun ((x, cy), c) -> cy = y), (fun s -> s |> Seq.minBy (fun ((x, y), c) -> x))

    grid |> Map.toSeq |> Seq.filter filter |> agg

let delta dir =
    match dir with
    | North -> (0, -1)
    | South -> (0, 1)
    | West -> (-1, 0)
    | East -> (1, 0)

let getNeighborCell (x, y) dir grid =
    let (dx, dy) = delta dir
    let nextPos = (x + dx, y + dy)

    match grid |> Map.tryFind nextPos with
    | Some c -> nextPos, c
    | None ->
        let wrapped = grid |> getWrappedCell (x, y) dir
        wrapped

let rec move grid instructions pos dir =
    let rec inner pos amount =
        if amount = 0 then
            pos
        else
            let nextPos, next = grid |> getNeighborCell pos dir

            match next with
            | Space -> inner nextPos (amount - 1)
            | Wall -> pos

    match instructions with
    | [] -> pos, dir
    | i :: rest ->
        match i with
        | Move amount -> move grid rest (inner pos amount) dir
        | Face direction -> move grid rest pos (turn dir direction)

let facing dir =
    match dir with
    | South -> 1
    | East -> 0
    | West -> 2
    | North -> 3

let part1 input =
    let grid, instructions = parse input
    let startPos = grid |> start |> fst
    let startDir = East

    let (x, y), orientation = move grid instructions startPos startDir

    (1000 * (y + 1)) + ((x + 1) * 4) + facing orientation

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day22.data
              Expect.equal subject 1 ""
          }

          test "sample" {
              let sampleOutput = part1 Day22.sample
              Expect.equal sampleOutput 6032 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
