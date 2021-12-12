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

let input = lines


let add1BitCount (index: int) (counts: int []) (line: char []) =
    if line.[index] = '1' then
        Array.set counts index (counts.[index] + 1)
        counts
    else
        counts

let all1BitCounts (seq: seq<char []>) (counts: int []) (index: int) =
    seq |> Seq.fold (add1BitCount index) counts

let toDecimal (bits: int []) =
    [| 16; 8; 4; 2; 1 |]
    |> Array.mapi (fun index value -> value * bits.[index])
    |> Array.sum

let solve (input: seq<string>) =
    let charLines =
        input |> Seq.map (fun line -> line.ToCharArray())

    let inputLength = Seq.length input

    let gammaRate =
        [| 0; 1; 2; 3; 4 |]
        |> Array.fold (all1BitCounts charLines) (Array.zeroCreate 5)
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
