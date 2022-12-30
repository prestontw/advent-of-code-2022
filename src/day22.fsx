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

let oppositeDirection dir =
    match dir with
    | North -> South
    | South -> North
    | West -> East
    | East -> West


let getWrappedCell (x, y) dir faceLength grid =
    let twoWestX = 0
    let twoNorthY = faceLength * 2
    let twoSouthY = faceLength * 3 - 1
    let oneWestX = 0
    let oneSouthY = faceLength * 4 - 1
    let oneNorthY = faceLength * 3
    let oneEastX = faceLength - 1
    let threeSouthY = faceLength * 3 - 1
    let threeEastX = faceLength * 2 - 1
    let threeWestX = faceLength
    let sixWestX = faceLength
    let sixEastX = faceLength * 2 - 1
    let sixNorthY = faceLength
    let fourWestX = faceLength
    let fourNorthY = 0
    let fourSouthY = faceLength - 1
    let fiveNorthY = 0
    let fiveEastX = faceLength * 3 - 1
    let fiveWestX = faceLength * 2
    let fiveSouthY = faceLength - 1

    let xVal, yVal = x % faceLength, y % faceLength

    match x / faceLength, y / faceLength, dir with
    // Face 4
    // to 1
    | 1, 0, North -> (oneWestX, xVal + oneNorthY), East
    // to 2
    | 1, 0, West -> (twoWestX, twoSouthY - yVal), East
    // Face 5
    // to 1
    | 2, 0, North -> (oneWestX + xVal, oneSouthY), North
    // to 6
    | 2, 0, South -> (sixEastX, sixNorthY + xVal), West
    // to 3
    | 2, 0, East -> (threeEastX, threeSouthY - yVal), West
    // Face 6
    // to 2
    | 1, 1, West -> (yVal + twoWestX, twoNorthY), South
    // to 5
    | 1, 1, East -> (yVal + fiveWestX, fiveSouthY), North
    // Face 2
    // to 6
    | 0, 2, North -> (sixWestX, sixNorthY + xVal), East
    // to 4
    | 0, 2, West -> (fourWestX, fourSouthY - yVal), East
    // Face 3
    // to 1
    // oof, got this incorrect! used yval instead
    | 1, 2, South -> (oneEastX, oneNorthY + xVal), West
    // to 5
    | 1, 2, East -> (fiveEastX, fiveSouthY - yVal), West
    // Face 1
    // to 4
    | 0, 3, West -> (fourWestX + yVal, fourNorthY), South
    // to 5
    | 0, 3, South -> (fiveWestX + xVal, fiveNorthY), South
    // to 3
    | 0, 3, East -> (yVal + threeWestX, threeSouthY), North


let delta dir =
    match dir with
    | North -> (0, -1)
    | South -> (0, 1)
    | West -> (-1, 0)
    | East -> (1, 0)

let getNeighborCell (x, y) dir faceLength grid =
    let (dx, dy) = delta dir
    let nextPos = (x + dx, y + dy)

    match grid |> Map.tryFind nextPos with
    | Some c -> (nextPos, c), dir
    | None ->
        let wrapped, dir = grid |> getWrappedCell (x, y) dir faceLength
        (wrapped, grid |> Map.find wrapped), dir

let rec move grid instructions pos dir faceLength =
    let rec inner pos dir amount =
        if amount = 0 then
            pos, dir
        else
            let ((nextPos, next), newDir) = grid |> getNeighborCell pos dir faceLength

            match next with
            | Space -> inner nextPos newDir (amount - 1)
            | Wall -> pos, dir

    match instructions with
    | [] -> pos, dir
    | i :: rest ->
        match i with
        | Move amount ->
            let pos, dir = inner pos dir amount
            move grid rest pos dir faceLength
        | Face direction -> move grid rest pos (turn dir direction) faceLength

let facing dir =
    match dir with
    | South -> 1
    | East -> 0
    | West -> 2
    | North -> 3

let part1 input faceLength =
    let grid, instructions = parse input
    let startPos = grid |> start |> fst
    let startDir = East

    let (x, y), orientation = move grid instructions startPos startDir faceLength

    (1000 * (y + 1)) + ((x + 1) * 4) + facing orientation

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day22.data 50
              Expect.isLessThan subject 114399 ""
              Expect.equal subject 1 ""
          }

          //   test "sample" {
          //       let sampleOutput = part1 Day22.sample 4
          //       Expect.equal sampleOutput 5031 ""
          //   }

          ]

let main = runTestsWithCLIArgs [] [||] tests
