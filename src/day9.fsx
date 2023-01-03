#load "Common.fsx"
#load "../data/day9.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines

    lines
    |> Seq.map (spaces)
    |> Seq.map (fun [| direction; amount |] -> direction, int amount)

let areAdjacent (x1, y1) (x2, y2) =
    (absDiff x1 x2) <= 1 && (absDiff y1 y2) <= 1

let makeAdjacent toMove target =
    let (toX, toY) = toMove
    let (targetX, targetY) = target

    if toX = targetX then
        //move up or down
        if targetY > toY then (toX, toY + 1) else (toX, toY - 1)
    else if toY = targetY then
        // move left or right
        if targetX > toX then (toX + 1, toY) else (toX - 1, toY)
    else if toX < targetX && toY < targetY then
        (toX + 1, toY + 1)
    else if toX < targetX && toY > targetY then
        (toX + 1, toY - 1)
    else if toX > targetX && toY < targetY then
        (toX - 1, toY + 1)
    else if toX > targetX && toY > targetY then
        (toX - 1, toY - 1)
    else
        printfn "whoopsie"
        toMove


let rec propogateMovement positions newHead =
    match positions with
    | [] -> []
    | current :: _ when areAdjacent current newHead -> positions
    | current :: tail ->
        // move current adjacent to new head
        let movedCurrent = makeAdjacent current newHead
        // then recurse
        movedCurrent :: (propogateMovement tail movedCurrent)

let rec next (positions: (int * int) list) dir amount tailVisited =

    if amount = 0 then
        positions, tailVisited
    else
        let headX, headY = positions[0]

        let newHead =
            match dir with
            | "D" -> headX, headY - 1
            | "U" -> headX, headY + 1
            | "L" -> headX - 1, headY
            | "R" -> headX + 1, headY

        let tail = propogateMovement (positions |> List.tail) newHead
        next (newHead :: tail) dir (amount - 1) (Set.add (tail |> List.last) tailVisited)


let visited lines ropeLength =
    let initial = List.init ropeLength (fun _ -> (0, 0)), (Set [ 0, 0 ])

    let folder =
        fun (positions, visited) (dir, amount) -> next positions dir amount visited

    let _, visited = Seq.fold folder initial lines

    visited

let part input ropeLength =
    let lines = parse input

    let visited = visited lines ropeLength

    Seq.length visited

let part1 input = part input 2
let part2 input = part input 10

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day9.data
              Expect.equal subject 6486 ""
          }

          test "sample 2" {
              let subject =
                  part2
                      "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

              Expect.equal subject 36 ""
          }

          test "part 2" {
              let outpu = part2 Day9.data
              Expect.equal outpu 2678 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
