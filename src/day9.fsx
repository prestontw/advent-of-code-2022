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

let rec next positions dir amount tailVisited =

    if amount = 0 then
        positions, tailVisited
    else
        let headX, headY = head

        let newHead =
            match dir with
            | "D" -> headX, headY - 1
            | "U" -> headX, headY + 1
            | "L" -> headX - 1, headY
            | "R" -> headX + 1, headY

        if areAdjacent newHead tail then
            next newHead tail dir (amount - 1) tailVisited
        else
            next newHead head dir (amount - 1) (Set.add head tailVisited)

let visited lines =
    let initial = (0, 0), (0, 0), (Set [ 0, 0 ])

    let folder =
        fun (head, tail, visited) (dir, amount) -> next head tail dir amount visited

    let _, _, visited = Seq.fold folder initial lines

    visited

let part1 input =
    let lines = parse input

    let visited = visited lines

    Seq.length visited

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
                  part1
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

          ]

let main = runTestsWithCLIArgs [] [||] tests
