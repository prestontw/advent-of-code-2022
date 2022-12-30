#load "Common.fsx"
#load "../data/day23.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Cell =
    | Elf
    | Empty

let parse input =
    let grid = grid input

    grid
    |> Map.map (fun _k c ->
        match c with
        | '#' -> Elf
        | '.' -> Empty)

type Directions =
    | N
    | NW
    | NE
    | E
    | SE
    | S
    | SW
    | W

let dirDiffs dir =
    match dir with
    | N -> 0, -1
    | NW -> -1, -1
    | NE -> 1, -1
    | E -> 1, 0
    | SE -> 1, 1
    | S -> 0, 1
    | SW -> -1, 1
    | W -> -1, 0

type Movements =
    | North
    | South
    | West
    | East

type Action =
    | DoNothing of (int * int)
    | Move of source: (int * int) * dest: (int * int)

let getNeighbors pos neighbors grid =
    let x, y = pos

    neighbors
    |> Seq.map (
        dirDiffs
        >> (fun (dx, dy) -> (x + dx, y + dy))
        >> (fun t -> grid |> Map.tryFind t)
    )

let neighborsAreEmpty neighbors =
    neighbors
    |> Seq.forall (fun neighbor -> neighbor |> Option.map (fun cell -> cell = Empty) |> Option.defaultValue true)

let checkThenMove pos directionsIfEmpty timestamp grid =
    if
        grid
        |> getNeighbors pos [ N; S; E; W; NE; NW; SE; SW ]
        |> Seq.toList
        |> neighborsAreEmpty
    then
        DoNothing pos
    else
        let dirToMove =
            seq { 0 .. (directionsIfEmpty |> List.length) }
            |> Seq.map (fun i -> directionsIfEmpty[(i + timestamp) % (directionsIfEmpty |> List.length)])
            |> Seq.tryFind (fun (_dir, dirsToCheck) -> getNeighbors pos dirsToCheck grid |> neighborsAreEmpty)
            |> Option.map (fst >> dirDiffs)

        match dirToMove with
        | None -> DoNothing pos
        | Some dirToMove ->
            let dx, dy = dirToMove
            let x, y = pos
            Move(source = pos, dest = (x + dx, y + dy))

let advance checkedDirections timestamp grid =
    let actions =
        grid
        |> Map.toSeq
        |> Seq.filter (fun (_, cell) -> cell = Elf)
        |> Seq.map (fun (pos, _) -> checkThenMove pos checkedDirections timestamp grid)

    let proposedSpaces =
        actions
        |> Seq.filter (function
            | DoNothing _ -> false
            | Move _ -> true)
        |> Seq.countBy (fun action ->
            let (Move(source, dest)) = action
            dest)
        |> Map.ofSeq

    actions,
    actions
    |> Seq.map (function
        | DoNothing(pos) -> (pos, Elf)
        | Move(source, dest) ->
            if (proposedSpaces |> Map.find dest) > 1 then
                (source, Elf)
            else
                dest, Elf)
    |> Map.ofSeq

let finalGrid checkedDirections grid =
    let rec inner grid timestamp =
        let actions, grid = grid |> advance checkedDirections timestamp

        if
            actions
            |> Seq.forall (function
                | DoNothing _ -> true
                | _ -> false)
        then
            timestamp
        else
            inner grid (timestamp + 1)

    (inner grid 0) + 1

let part1 input =
    let lines = parse input
    // n s w e
    let checkedDirections =
        [ (N, [ N; NE; NW ])
          (S, [ S; SE; SW ])
          (W, [ W; SW; NW ])
          (E, [ E; NE; SE ]) ]

    lines |> finalGrid checkedDirections


let tests =
    testList
        "parts"
        [

          test "sample" {
              let subject = part1 Day23.sample
              Expect.equal subject 20 ""
          }

          test "part 1" {
              let subject = part1 Day23.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
