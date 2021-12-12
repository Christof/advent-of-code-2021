let exampleInput =
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"


let lines = exampleInput.Split '\n'

let input = System.IO.File.ReadLines("fs/day3.txt")


let add1BitCount (index: int) (counts: int []) (line: char []) =
    if line.[index] = '1' then
        Array.set counts index (counts.[index] + 1)
        counts
    else
        counts

let all1BitCounts (seq: seq<char []>) (counts: int []) (index: int) =
    seq |> Seq.fold (add1BitCount index) counts

let toDecimal (bits: int []) =
    let maxExp = (Array.length bits) - 1

    bits
    |> Array.mapi (fun index _ -> 1 <<< (maxExp - index))
    |> Array.mapi (fun index value -> value * bits.[index])
    |> Array.sum

let solve (input: seq<string>) =
    let charLines =
        input |> Seq.map (fun line -> line.ToCharArray())

    let inputLength = Seq.length input

    let wordLength = Array.length (Seq.head charLines)

    let gammaRate =
        Array.zeroCreate wordLength
        |> Array.mapi (fun index _ -> index)
        |> Array.fold (all1BitCounts charLines) (Array.zeroCreate wordLength)
        |> Array.mapi (fun index count1s ->
            if (count1s * 2 > inputLength) then
                1
            else
                0)

    let epsilonRate =
        gammaRate
        |> Array.map (fun bit -> if bit = 1 then 0 else 1)

    let result =
        (toDecimal gammaRate) * (toDecimal epsilonRate)

    printf "result is %d" result

let calculateMostCommonBits (charLines: seq<char []>) =
    let inputLength = Seq.length charLines

    let wordLength = Array.length (Seq.head charLines)

    Array.zeroCreate wordLength
    |> Array.mapi (fun index _ -> index)
    |> Array.fold (all1BitCounts charLines) (Array.zeroCreate wordLength)
    |> Array.mapi (fun index count1s ->
        if (count1s * 2 >= inputLength) then
            '1'
        else
            '0')

let calculateLeastCommonBits (charLines: seq<char []>) =
    let inputLength = Seq.length charLines

    let wordLength = Array.length (Seq.head charLines)

    Array.zeroCreate wordLength
    |> Array.mapi (fun index _ -> index)
    |> Array.fold (all1BitCounts charLines) (Array.zeroCreate wordLength)
    |> Array.mapi (fun index count1s ->
        if (count1s * 2 < inputLength) then
            '1'
        else
            '0')

let rec filterByBitCriteria (index: int) (values: seq<char []>) =
    let mostCommonBits = calculateMostCommonBits values

    let filtered =
        values
        |> Seq.filter (fun line -> line.[index] = mostCommonBits.[index])

    if Seq.length filtered = 1 then
        Seq.head filtered
    else
        filterByBitCriteria (index + 1) filtered

let rec filterByBitCriteria2 (index: int) (values: seq<char []>) =
    let leastCommonBits = calculateLeastCommonBits values

    let filtered =
        values
        |> Seq.filter (fun line -> (line.[index] = leastCommonBits.[index]))

    if Seq.length filtered = 1 then
        Seq.head filtered
    else
        filterByBitCriteria2 (index + 1) filtered

let solve2 (input: seq<string>) =
    let charLines =
        input |> Seq.map (fun line -> line.ToCharArray())


    let oxygenGeneratorRating =
        filterByBitCriteria 0 charLines
        |> Array.map (fun value -> if value = '1' then 1 else 0)

    let co2ScrubberRating =
        filterByBitCriteria2 0 charLines
        |> Array.map (fun value -> if value = '1' then 1 else 0)

    printf "oxygen generator rating is %A\nco2 scrubbing rating is %A\n" oxygenGeneratorRating co2ScrubberRating

    let result =
        (toDecimal oxygenGeneratorRating)
        * (toDecimal co2ScrubberRating)

    printf "result is %d" result
