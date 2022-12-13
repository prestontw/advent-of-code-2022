#load "Common.fsx"
#load "../data/day13.fsx"
#r "nuget: Expecto"

open Common
open Expecto

type SExpr =
    | Word of string
    | Sentence of SExpr list

type Packet =
    | Int of int
    | PList of Packet list

let parseSExpr (line: string) =
    let rec inner (sentence: SExpr list) (pendingWord: string option) (remaining: string) =
        if remaining = "" then

            Sentence sentence, ""
        else
            let tail = remaining[1..]

            match remaining[0] with
            | '[' ->
                let innerSentence, tail = inner [] None tail
                let sentence = List.append sentence [ innerSentence ]
                inner sentence None tail
            | ']' ->
                let sentence =
                    match pendingWord with
                    | Some s -> List.append sentence [ Word s ]
                    | None -> sentence

                (Sentence sentence), tail
            | ',' ->
                inner
                    (List.append
                        sentence
                        (pendingWord |> Option.map (fun word -> [ Word word ]) |> Option.defaultValue []))
                    None
                    tail
            | c ->
                inner
                    sentence
                    (pendingWord
                     |> Option.map (fun existing -> existing + string c)
                     |> Option.orElse (Some(string c)))
                    tail

    // take off the first '[' because we start with a list already
    inner [] None line[1..]

let rec parsePacket sexpr =
    match sexpr with
    | Word s -> Int(int s)
    | Sentence words -> words |> List.map parsePacket |> PList

let parse input =
    let lines = input |> blankLines

    lines
    |> Seq.map (fun pair ->
        let [| l; r |] = pair |> Array.map (parseSExpr >> fst >> parsePacket)
        l, r)

let rec lLessThanR lPacket rPacket =
    match lPacket, rPacket with
    | Int l, Int r when l < r -> Some true
    | Int l, Int r when l > r -> Some false
    | Int _, Int _ -> None
    | PList(l :: remainingL), PList(r :: remainingR) ->
        match lLessThanR l r with
        | None -> lLessThanR (PList remainingL) (PList remainingR)
        | a -> a
    | PList [], PList(_ :: _) -> Some true
    | PList(_ :: _), PList [] -> Some false
    | PList [], PList [] -> None
    | PList l, Int r -> lLessThanR (PList l) (PList [ Int r ])
    | Int l, PList r -> lLessThanR (PList [ Int l ]) (PList r)


let part1 input =
    let packetPairs = parse input

    packetPairs
    |> Seq.indexed
    |> Seq.choose (fun (i, (l, r)) ->
        if (lLessThanR l r |> Option.defaultValue false) then
            Some(i + 1)
        else
            None)
    |> Seq.sum

let tests =
    testList
        "parts"
        [

          test "parse" {
              let output = "[[],[1,2],3]" |> parseSExpr |> fst |> parsePacket
              let expected = PList([ PList []; PList [ Int 1; Int 2 ]; Int 3 ])
              Expect.equal output expected ""
          }

          test "sample" {
              let output =
                  part1
                      "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"

              Expect.equal output 13 ""
          }

          test "part 1" {
              let subject = part1 Day13.data
              Expect.equal subject 1 ""
          }

          ]

let main = runTestsWithCLIArgs [] [||] tests
