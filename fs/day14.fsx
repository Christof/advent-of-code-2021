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

let simulateStep (polymer: string) (rules: array<string>) =
    let mutable output = polymer.Substring(0, 1)

    for i in [ 0 .. (polymer.Length) - 2 ] do
        let pair = polymer.Substring(i, 2)

        let ruleIndex =
            rules
            |> Array.findIndex (fun rule -> rule.StartsWith(pair))

        if (ruleIndex >= 0) then
            let rule = rules.[ruleIndex]

            output <-
                output
                + rule.Substring(rule.Length - 1)
                + (string pair.[1])
        else
            printf "\n no rule found\n"
            output <- output + (string pair.[1])

    output



let solve (input: seq<string>) (steps: int) =
    let polymerTemplate = Seq.head input

    let pairInsertionRules = input |> Seq.skip 2 |> Seq.toArray

    let polymer =
        { 1 .. steps }
        |> Seq.fold (fun p i -> simulateStep p pairInsertionRules) polymerTemplate

    printf "%s\n" polymer

    let frequencies =
        polymer.ToCharArray()
        |> Array.groupBy id
        |> Array.map (fun (c, occ) -> (c, Seq.length occ))

    let max = frequencies |> Array.maxBy snd |> snd
    let min = frequencies |> Array.minBy snd |> snd

    let diff = max - min
    printf "frequencies %A\nresult %d\n" frequencies diff

    diff

solve lines 10 // 1588
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
solve input 10 // 2915
stopWatch.Stop()
printfn "time: %f ms" stopWatch.Elapsed.TotalMilliseconds

// solve lines 40 // 2188189693529