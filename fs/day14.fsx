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


    let initialState = [ (polymerTemplate.ToCharArray()) ]

    let mutable splitPolymer = Array.empty

    let polymer =
        { 1 .. steps }
        |> Seq.fold
            (fun p i ->
                printf "%d\n" i

                if (List.length p = 1)
                   && (Array.length (List.head p) > (System.Int32.MaxValue / 64)) then
                    let splits =
                        (List.head p) |> Array.windowed 2 |> Array.toList

                    printf "split with %A" splits
                    splitPolymer <- (List.head p)

                    splits
                    |> List.map (fun s -> simulateStep s rulesMap)
                else
                    p |> List.map (fun s -> simulateStep s rulesMap))
            initialState

    printf "\nfinal: \n%A\n" polymer

    let frequencies =
        polymer
        |> List.map (fun p -> p |> Array.countBy id)
        |> List.reduce (fun a b -> Array.append a b)
        |> Array.groupBy fst
        |> Array.map (fun (c, g) -> (c, g |> Array.sumBy snd))


    let toSubtract =
        if (Array.length splitPolymer > 0) then
            Array.sub splitPolymer 1 (splitPolymer.Length - 2)
        else
            Array.empty

    let toSubtractFrequencies = toSubtract |> Array.countBy id

    printf "splitPolymer: %A\n" splitPolymer

    let final =
        frequencies
        |> Array.map (fun (char, count) ->
            printf "%c, " char

            match Array.tryFind (fun (c, _) -> c = char) toSubtractFrequencies with
            | Some (_, sub) -> (char, count - sub)
            | None -> (char, count))

    let max = final |> Array.maxBy snd |> snd
    let min = final |> Array.minBy snd |> snd

    let diff = max - min
    printf "frequencies %A\nresult %d\n" frequencies diff

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
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
solve input 12 // 12085
stopWatch.Stop()
printfn "time: %f ms" stopWatch.Elapsed.TotalMilliseconds

solve lines 40 // 2188189693529
