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

let solve (input: seq<string>) =
    let array =
        input
        |> Seq.map (fun line -> line.ToCharArray())
        |> Seq.fold (add1BitCount 0) (Array.zeroCreate 5)

    printf "result is %d" array.[0]
