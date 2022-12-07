#load "Common.fsx"
#load "../data/day7.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    Seq.map (spaces) lines

type FileSystem =
    { Children: Map<string, FileSystem>
      Files: Map<string, int> }

let newFiles = { Children = Map []; Files = Map [] }

let part1 input =
    let lines = parse input

    let rec traverse (tree: FileSystem) (command: list<string[]>) =
        let { Children = children; Files = files } = tree

        match command with
        | current :: rest ->
            match current[0], current[1] with
            | "$", "cd" ->
                match current[2] with
                | ".." -> tree, rest
                | child ->
                    let inner, remaining = traverse newFiles rest
                    let newDirs = children |> Map.add child inner
                    traverse { Children = newDirs; Files = files } remaining
            | "$", "ls" -> traverse tree rest
            | "dir", _entry -> traverse tree rest
            | size, fileName ->
                let size = int size
                let newFiles = files |> Map.add fileName size

                traverse
                    { Children = children
                      Files = newFiles }
                    rest
        | [] -> tree, []

    let fileSystem = traverse newFiles (lines |> Seq.toList)
    fileSystem |> Map.toSeq |> Seq.length

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day7.data
              Expect.equal subject 1 ""
          }

          test "part 2" {
              let subject = part1 Day7.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
