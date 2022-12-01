open System.Text.RegularExpressions

let manhattanDistance (x, y) = abs x + abs y

let absDiff x y = (max x y) - (min x y)

let keysToSet (m: Map<'a, 'b>) : Set<'a> = m |> Map.keys |> Set.ofSeq

let values m = m |> Map.values

let counts (s: seq<'a>) : Map<'a, int> = Seq.countBy id s |> Map.ofSeq

let digits (i: int) =
    i |> string |> Seq.map (fun i -> int i - int '0')

let lines (i: string) = i.Split '\n'

/// This returns in reverse order
let reversedBlankLines (i: string) =
    let (groups, last) =
        i
        |> lines
        |> Array.fold
            (fun (groups, currentGroup) line ->
                if line.Length = 0 then
                    (currentGroup :: groups, List.empty)
                else
                    (groups, line :: currentGroup))
            (List.empty, List.empty)

    last :: groups

let blankLines (i: string) =
    i
    |> reversedBlankLines
    |> List.map List.rev
    |> List.rev

let commas (i: string) = i.Split ','

let spaces (i: string) = i.Split ' '

let xor a b = (a && not b) || (not a && b)

/// Memoize the function `f`.
/// ```
/// let rec fib n =
///     if n = 0 || n = 1 then 1 else fastFib (n - 1) + fastFib (n - 2)
/// and fastFib = memoize fib
/// ```
let memoize f =
    let savedResults = ref Map.empty

    fun input ->
        match Map.tryFind input savedResults.Value with
        | Some result -> result
        | None ->
            printf "computing from scratch! %A\n" input
            let result = f input
            savedResults.Value <- Map.add input result savedResults.Value
            result

let extractValues regex str =
    let attempt = Regex(regex).Match str

    if attempt.Success then
        attempt.Groups
        |> Seq.map (fun x -> x.Value)
        |> Seq.skip 1
        |> Some
    else
        None

let extractNamedValues regex str =
    let attempt = Regex(regex).Match str

    if attempt.Success then
        attempt.Groups
        |> Seq.map (fun x -> x.Name, x.Value)
        |> Seq.skip 1
        |> Some
    else
        None
