let exampleInput =
    "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let lines = exampleInput.Split('\n')

let input =
    System.IO.File.ReadLines("inputs/day14.txt")

let simulateStep (polymer: array<char>) (rules: array<string>) =
    let output =
        Array.create (2 * polymer.Length - 1) ' '

    Array.set output 0 polymer.[0]

    let mutable index = 1

    for pairChar in (polymer |> Array.windowed 2) do
        let pair = pairChar |> System.String

        let rule =
            rules
            |> Array.find (fun rule -> rule.StartsWith(pair))

        Array.set output index (rule.Substring(rule.Length - 1).ToCharArray().[0])
        Array.set output (index + 1) (pairChar.[1])

        index <- index + 2


    output



let solve (input: seq<string>) (steps: int) =
    let polymerTemplate = Seq.head input

    let pairInsertionRules = input |> Seq.skip 2 |> Seq.toArray

    let polymer =
        { 1 .. steps }
        |> Seq.fold (fun p i -> simulateStep p pairInsertionRules) (polymerTemplate.ToCharArray())

    printf "%A\n" polymer

    let frequencies = polymer |> Array.countBy id

    let max = frequencies |> Array.maxBy snd |> snd
    let min = frequencies |> Array.minBy snd |> snd

    let diff = max - min
    printf "frequencies %A\nresult %d\n" frequencies diff

    diff

solve lines 10 // 1588
// solve input 10 // 2915
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
solve input 12 // 12085
stopWatch.Stop()
printfn "time: %f ms" stopWatch.Elapsed.TotalMilliseconds

// solve lines 40 // 2188189693529
