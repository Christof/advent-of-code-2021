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


let mergeMaps (a: Map<char, uint64>) (b: Map<char, uint64>) =
    b
    |> Map.fold
        (fun acc key value ->
            if Map.containsKey key acc then
                Map.add key (acc.[key] + value) acc
            else
                Map.add key value acc)
        a



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


    let polymerHalfFinished =
        { 1 .. (steps / 2) }
        |> Seq.fold
            (fun p i ->
                printf "%d\n" i

                simulateStep p rulesMap)
            (polymerTemplate.ToCharArray())


    let splits =
        polymerHalfFinished
        |> Array.windowed 2
        |> Array.groupBy id
        |> Array.map (fun (window, occurences) -> (window, occurences.Length))

    printf "split with %d with %d unique\n%A\n" polymerHalfFinished.Length splits.Length splits

    let polymer =
        splits
        |> Array.Parallel.map (fun (s, count) ->
            { (steps / 2 + 1) .. steps }
            |> Seq.fold
                (fun p i ->
                    // printf "%d\n" i

                    simulateStep p rulesMap)
                s
            |> Seq.countBy id
            |> Map
            |> Map.map (fun _key value -> (uint64 value) * (uint64 count)))



    let frequencies =
        polymer
        |> Array.fold mergeMaps Map.empty<char, uint64>


    let toSubtract =
        Array.sub polymerHalfFinished 1 (polymerHalfFinished.Length - 2)

    let toSubtractFrequencies = toSubtract |> Array.countBy id |> Map

    printf "splitPolymer: %A\n" polymerHalfFinished

    let final =
        toSubtractFrequencies
        |> Map.fold
            (fun acc key value ->
                if Map.containsKey key acc then
                    Map.add key (acc.[key] - (uint64 value)) acc
                else
                    Map.add key (uint64 value) acc)
            frequencies

    let max = final |> Map.values |> Seq.max
    let min = final |> Map.values |> Seq.min

    let diff = max - min
    printf "frequencies %A\nmax: %d min: %d\nresult %d\n" frequencies max min diff

    diff


let rec simulateLengths (length: uint64) (remainingSteps: int) =
    if (remainingSteps = 0) then
        length
    else
        simulateLengths (length * (uint64 2) - (uint64 1)) (remainingSteps - 1)

(simulateLengths (uint64 20) 40)
/ (uint64 System.Int32.MaxValue)
/ (uint64 System.Int32.MaxValue)

solve lines 10 // 1588
solve input 10 // 2915
solve input 4 // 42
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
solve input 12 // 12085
stopWatch.Stop()
printfn "time: %f ms" stopWatch.Elapsed.TotalMilliseconds

solve lines 40 // 2188189693529
solve input 40 // 3353146900153
