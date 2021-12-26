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

let inline getKey (a: char) (b: char) = (int a) * 256 + (int b)

let simulateStep (polymer: array<char>) (rules: Map<int, char>) =
    let output =
        Array.create (2 * polymer.Length - 1) ' '

    Array.set output 0 polymer.[0]

    for i = 0 to (polymer.Length - 2) do
        let char =
            rules.[getKey polymer.[i] polymer.[i + 1]]

        Array.set output (i * 2 + 1) char
        Array.set output (i * 2 + 2) (polymer.[i + 1])


    output



let solve (input: seq<string>) (steps: int) =
    let polymerTemplate = Seq.head input

    let pairInsertionRules = input |> Seq.skip 2 |> Seq.toArray

    let rulesMap =
        pairInsertionRules
        |> Array.map (fun rule ->
            let startChars = rule.Substring(0, 2).ToCharArray()

            let toChar =
                rule.Substring(rule.Length - 1).ToCharArray().[0]

            (getKey startChars.[0] startChars.[1], toChar))
        |> Map


    let polymer =
        { 1 .. steps }
        |> Seq.fold (fun p i -> simulateStep p rulesMap) (polymerTemplate.ToCharArray())

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
