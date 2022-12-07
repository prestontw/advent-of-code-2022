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

let newFiles = (Directory { Children = Map [] })
let newDirectory = { Children = Map [] }

// this isn't doing what I would expect it to do
let sizes fs =

    let rec inner dir (prefix: string) (sizes: Map<string, int>) =
        let folder (acc) name child =
            match child with
            | File size -> (acc + size, sizes)
            | Directory d ->
                let newPrefix = (prefix + "/" + name)
                let size, newSizes = inner d newPrefix sizes
                (acc, newSizes |> Map.add newPrefix size)

        Map.fold folder (0, (Map [])) dir.Children

    inner fs "/" (Map []) |> snd

let part1 input =
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
    fileSystem
