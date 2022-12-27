#load "Common.fsx"
#load "../data/day16.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let lineRegex = "Valve (.*) has flow rate=(\d+); tunnels? leads? to valves? (.*)"
let matchLine = extractValues lineRegex

type Valve =
    { name: string
      flowRate: int
      neighbors: string array
      isOpen: bool }

let parse input =
    let lines = input |> lines

    let parseLine line =
        let [ valveName; flowRate; neighbors ] = matchLine line |> Option.get |> Seq.toList
        let flowRate = int flowRate
        let neighbors = neighbors.Split ", "

        { name = valveName
          flowRate = flowRate
          neighbors = neighbors
          isOpen = false }

    Seq.map parseLine lines

let mapByName valves =
    valves |> Seq.map (fun v -> (v.name, v)) |> Map.ofSeq

let bfs valves start =
    let rec inner queue seen =
        if queue |> Seq.isEmpty then
            seen
        else
            let (valve, cost) = queue |> Seq.head

            let (queue, seen) =
                valve.neighbors
                |> Seq.fold
                    (fun (queue, seen) cur ->
                        let cur = valves |> Map.find cur

                        if seen |> Map.containsKey cur then
                            (queue, seen)
                        else
                            (Seq.append queue [ cur, cost + 1 ]), (seen |> Map.add cur (cost + 1)))
                    ((queue |> Seq.tail), seen)

            inner queue seen

    inner (seq { start, 0 }) (Map [ start, 0 ])

let openingCost = 1

let calculate valves start timeLength =
    let rec inner location time totalFlow valves =
        let costsToOthers = bfs valves location

        let (nextLocation, flowAdded) =
            costsToOthers
            |> Map.toSeq
            |> Seq.filter (fun (v, _) -> not v.isOpen)
            |> Seq.map (fun (valve, stepsThere) ->
                valve, valve.flowRate * (timeLength - (time + stepsThere + openingCost)))
            |> Seq.sortDescending
            |> Seq.head

        inner
            nextLocation
            (time + (costsToOthers |> Map.find nextLocation) + openingCost)
            (totalFlow + flowAdded)
            (valves |> Map.add nextLocation.name { nextLocation with isOpen = true })

    inner (valves |> Map.find start) 0 0 valves


let part1 input =
    let lines = parse input
    let valves = mapByName lines

    calculate valves "AA" 30

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day16.data
              Expect.equal subject 1 ""
          }

          test "sample" {
              let subject = part1 Day16.sample
              Expect.equal subject 1651 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
