#load "Common.fsx"
#load "../data/day19.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type Blueprint =
    { num: int
      oreRCost: int
      clayRCost: int
      obsRCost: int * int
      geodeRCost: int * int }

let lineRegex =
    "Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."

let matchLine = extractValues lineRegex

let parse input =
    let lines = input |> lines

    Seq.map
        (fun line ->
            let [ num; orecost; claycost; obsOreCost; obsClayCost; geodeOreCost; geodeObsCost ] =
                line |> matchLine |> Option.get |> Seq.map int |> Seq.toList

            { num = num
              oreRCost = orecost
              clayRCost = claycost
              obsRCost = obsOreCost, obsClayCost
              geodeRCost = geodeOreCost, geodeObsCost })
        lines

type Action =
    | Nothing
    | MakeOreRobot
    | MakeClayRobot
    | MakeObsidianRobot
    | MakeGeodeRobot

type ResourceCounts =
    { ore: int
      clay: int
      obsidian: int
      geode: int
      oreRobot: int
      clayRobot: int
      obsidianRobot: int
      geodeRobot: int }

let emptyCounts =
    { ore = 0
      clay = 0
      obsidian = 0
      geode = 0
      oreRobot = 1
      clayRobot = 0
      obsidianRobot = 0
      geodeRobot = 0 }

let robotBuild counts =
    { counts with
        ore = counts.ore + counts.oreRobot
        clay = counts.clay + counts.clayRobot
        obsidian = counts.obsidian + counts.obsidianRobot
        geode = counts.geode + counts.geodeRobot }

let act action counts blueprint =
    match action with
    | Nothing -> counts |> robotBuild
    | MakeClayRobot when counts.ore >= blueprint.clayRCost ->
        let counts = counts |> robotBuild

        { counts with
            ore = counts.ore - blueprint.clayRCost
            clayRobot = counts.clayRobot + 1 }
    | MakeClayRobot -> counts |> robotBuild
    | MakeOreRobot when counts.ore >= blueprint.oreRCost ->
        let counts = counts |> robotBuild

        { counts with
            ore = counts.ore - blueprint.oreRCost
            oreRobot = counts.oreRobot + 1 }
    | MakeOreRobot -> counts |> robotBuild
    | MakeGeodeRobot when
        (counts.ore >= (blueprint.geodeRCost |> fst))
        && counts.obsidian >= (blueprint.geodeRCost |> snd)
        ->
        let counts = counts |> robotBuild

        { counts with
            ore = counts.ore - (blueprint.geodeRCost |> fst)
            obsidian = counts.obsidian - (blueprint.geodeRCost |> snd)
            geodeRobot = counts.geodeRobot + 1 }
    | MakeGeodeRobot -> counts |> robotBuild
    | MakeObsidianRobot when
        counts.ore >= (blueprint.obsRCost |> fst)
        && counts.clay >= (blueprint.obsRCost |> snd)
        ->
        let counts = counts |> robotBuild

        { counts with
            ore = counts.ore - (blueprint.obsRCost |> fst)
            clay = counts.clay - (blueprint.obsRCost |> snd)
            obsidianRobot = counts.obsidianRobot + 1 }
    | MakeObsidianRobot -> counts |> robotBuild

let actM = memoize (fun (t, c, b) -> act t c b)

let actions =
    seq {
        MakeGeodeRobot
        MakeObsidianRobot
        MakeClayRobot
        MakeOreRobot
        Nothing
    }

let calculateGeodes blueprint =

    let rec inner (time, counts) =
        if time >= 24 then
            counts.geode
        else
            actions
            |> Seq.map (fun action ->
                let counts = act action counts blueprint
                innerFast ((time + 1), counts))
            |> Seq.max

    and innerFast = memoize inner

    inner (0, emptyCounts)

let part1 input =
    let blueprints = parse input

    let qualityLevel blueprint =
        tee blueprint.num * tee (calculateGeodes blueprint)

    blueprints |> Seq.sumBy qualityLevel

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day19.data
              Expect.equal subject 33 ""
          }
          //   test "bounds" {
          //       let parsed = Day19.sample |> parse |> Seq.toList |> List.head

          //       let subject = calculateGeodes parsed
          //       Expect.equal subject 9 ""
          //   }

          ]

let main = runTestsWithCLIArgs [] [||] tests
