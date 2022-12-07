#load "Common.fsx"
#load "../data/day7.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let parse input =
    let lines = input |> lines
    Seq.map (spaces) lines

type FileEntries =
    | File of int
    | Directory of Directory

and Directory = { Children: Map<string, FileEntries> }

let newDirectory = { Children = Map [] }

let directorySizes fs root =
    let rec directoryAndInnerSizes fs path sizes =
        let folder (size, innerSizes) name entry =
            match entry with
            | File s -> (size + s, innerSizes)
            | Directory d ->
                let (innerSize, childrenSizes) =
                    directoryAndInnerSizes d (path + name + "/") innerSizes

                (innerSize + size, childrenSizes)

        let (size, innerSizes) = fs.Children |> Map.fold folder (0, sizes)
        (size, innerSizes |> Map.add path size)

    let rootEntry = fs.Children[root]

    match rootEntry with
    | File i -> i, (Map [])
    | Directory d -> directoryAndInnerSizes d root (Map [])


let sizes input =
    let lines = parse input

    let rec traverse (tree: Directory) (command: list<string[]>) =
        let { Children = children } = tree

        match command with
        | current :: rest ->
            match current[0], current[1] with
            | "$", "cd" ->
                match current[2] with
                | ".." -> tree, rest
                | child ->
                    let inner, remaining = traverse newDirectory rest
                    let newDirs = children |> Map.add child (Directory(inner))
                    traverse { Children = newDirs } remaining
            | "$", "ls" -> traverse tree rest
            | "dir", _entry -> traverse tree rest
            | size, fileName ->
                let size = int size
                let newFiles = children |> Map.add fileName (File size)

                traverse { Children = newFiles } rest
        | [] -> tree, []

    let fileSystem, [] = traverse newDirectory (lines |> Seq.toList)
    let filledSize, dirsAndSizes = directorySizes fileSystem "/"


    (filledSize, dirsAndSizes |> Map.toSeq |> Seq.map snd)

let part1 input =
    let (_totalSize, sizes) = sizes input
    sizes |> Seq.filter (fun size -> size < 100000) |> Seq.sum

let totalSize = 70000000
let neededUnusedSize = 30000000

let part2 input =
    let (filledSize, sizes) = sizes input
    let currentUnusedSpace = totalSize - filledSize

    sizes
    |> Seq.sort
    |> Seq.find (fun size -> currentUnusedSpace + size >= neededUnusedSize)

let tests =
    testList
        "parts"
        [

          test "part 1" {
              let subject = part1 Day7.data
              Expect.equal subject 1845346 ""
          }

          test "part 2" {
              let subject = part2 Day7.data
              Expect.equal subject 3636703 ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests
