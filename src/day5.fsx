#load "Common.fsx"
#load "../data/day5.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let instructionReg = "move (\d+) from (\d+) to (\d+)"
let matchInstruction = Common.extractValues instructionReg

let parse (input: string) =
    let starting =
        [| "FGVRJLD"
           "SJHVBMPT"
           "CPGDFMHV"
           "QGNPDM"
           "FNHLJ"
           "ZTGDQVFN"
           "LBDF"
           "NDVSBJM"
           "DLG" |]

    let starting = starting |> Array.map (Seq.toArray)

    let [| _starting; instructions |] = input.Split "\n\n"

    starting,
    lines instructions
    |> Seq.map (matchInstruction >> Option.get)
    |> Seq.map Seq.toArray
    |> Seq.map (Array.map int)
    |> Seq.map (fun [| amount; from; to_ |] -> [| amount; from - 1; to_ - 1 |])
    |> Seq.toArray


let crane (starting, instructions) insertion =
    let folder (state: char[][]) [| (count: int); (from: int); to_ |] =
        let incoming = state[from][0 .. (count - 1)]
        Array.set state from (state[from][count..])
        Array.set state to_ (Array.append (insertion incoming) state[to_])
        state

    Seq.fold folder starting instructions
    |> Seq.map (fun a -> Array.get a 0)
    |> Seq.toArray
    |> System.String

let part1 input =
    let starting, instructions = parse input

    crane (starting, instructions) Array.rev

let part2 input =
    let starting, instructions = parse input

    crane (starting, instructions) id

let tests =
    testList
        "parts"
        [ test "part 1" {
              let subject = part1 Day5.data
              Expect.equal subject "QMBMJDFTD" ""
          }

          test "part 2" {
              let subject = part2 Day5.data
              Expect.equal subject "NBTVTJNFJ" ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests
