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

let bfs valves (start: string) =
    let rec inner queue seen =
        if queue |> Seq.isEmpty then
            seen
        else
            let (valveName, cost) = queue |> Seq.head

            let valve = valves |> Map.find valveName

            let (queue, seen) =
                valve.neighbors
                |> Seq.fold
                    (fun (queue, seen) cur ->
                        let curValve = valves |> Map.find cur

                        if seen |> Map.containsKey cur then
                            (queue, seen)
                        else
                            (Seq.append queue [ cur, cost + 1 ]), (seen |> Map.add cur (cost + 1)))
                    ((queue |> Seq.tail), seen)

            inner queue seen

    inner (seq { start, 0 }) (Map [ start, 0 ])

let openingCost = 1

type Releaser =
    | AwakenAt of int * string
    | Done

let releasedTime releaser =
    match releaser with
    | AwakenAt(time, _) -> Some time
    | Done -> None

let releasedLocation releaser =
    match releaser with
    | AwakenAt(_, loc) -> Some loc
    | Done -> None

let optimisticMaxFlow currentMax valves (timeSoFar, totalTime) (human, elephant) =
    let remainingTaskFactor =
        match human, elephant with
        | AwakenAt _, AwakenAt _ -> 1
        | AwakenAt _, Done
        | Done, AwakenAt _ -> 2
        | Done, Done -> 1000 // some large number since no more work will be done

    let valveValues =
        valves
        |> Seq.filter (fun valve -> not valve.isOpen)
        |> Seq.sortByDescending (fun valve -> valve.flowRate)
        |> Seq.indexed
        |> Seq.map (fun (index, valve) ->
            let timeOpen = max 0 ((totalTime - timeSoFar - (index + 1) * 2))
            valve.flowRate * timeOpen)

    currentMax
    + (valveValues
       |> Seq.truncate ((totalTime - timeSoFar - 1) / remainingTaskFactor)
       |> Seq.sum)

let maxFlow valves start timeLength =
    let mutable currentMax = None

    // these releasers are in sorted order!
    // if the first releaser is done, second releaser is also done!
    let rec inner (releasers, valves: Map<string, Valve>, totalFlow) =
        let human, elephant = releasers

        if (human, elephant) = (Done, Done) then
            totalFlow
        else if (releasedTime human |> Option.get) >= timeLength then
            if currentMax |> Option.map (fun m -> totalFlow > m) |> Option.defaultValue true then
                currentMax <- Some totalFlow

            totalFlow
        else if
            currentMax
            |> Option.map (fun m ->
                optimisticMaxFlow
                    totalFlow
                    (valves |> Map.values)
                    (releasedTime human |> Option.get, timeLength)
                    (human, elephant)
                <= m)
            |> Option.defaultValue false
        then
            totalFlow
        else
            // pick next places to go, sorted by flow value
            let worthwhileDestinations =
                valves
                |> Map.values
                |> Seq.filter (fun v -> v.flowRate <> 0)
                |> Seq.filter (fun v -> not v.isOpen)
                |> Seq.sortByDescending (fun v -> v.flowRate)

            if worthwhileDestinations |> Seq.isEmpty then
                totalFlow
            else

                let time = releasedTime human |> Option.get

                let costsToOthers: Map<string, int> =
                    bfs valves (releasedLocation human |> Option.get)

                worthwhileDestinations
                |> Seq.map (fun destination ->
                    let timeToDestination = costsToOthers |> Map.find destination.name

                    let nextTimestamp = timeToDestination + time + 1
                    let flowAdded = max 0 (destination.flowRate * (timeLength - nextTimestamp))

                    let doneTotalFlow = totalFlow
                    let totalFlow = totalFlow + flowAdded

                    let nextHuman = AwakenAt(nextTimestamp, destination.name)

                    let nextReleasers =
                        match elephant with
                        | Done -> nextHuman, Done
                        | AwakenAt(newerTime, _) when newerTime < nextTimestamp -> elephant, nextHuman
                        | AwakenAt _ -> nextHuman, elephant

                    let humanTakeFlowReleased =
                        innerFast (
                            nextReleasers,
                            valves |> Map.add destination.name { destination with isOpen = true },
                            totalFlow
                        )

                    let humanDone = innerFast ((snd nextReleasers, Done), valves, doneTotalFlow)
                    max humanTakeFlowReleased humanDone)
                |> Seq.max

    and innerFast = memoize inner


    inner ((AwakenAt(0, start), AwakenAt(0, start)), valves, 0)


let calculate valves start timeLength = maxFlow valves start timeLength


let part1 input =
    let lines = parse input
    let valves = mapByName lines

    calculate valves "AA" 26

let tests =
    testList
        "parts"
        [

          test "part 1" { let subject = part1 Day16.data
                          Expect.equal subject 1673 ""
          }

          test "sample" {
              let subject = part1 Day16.sample
              Expect.equal subject 1707 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
