let example1 = "D2FE28"

let input =
    System.IO.File.ReadLines("inputs/day16.txt")

let hexToBin (hex: char) =
    match hex with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> raise (System.ArgumentException("Invalid character"))

let binToDec (bin: string) = System.Convert.ToInt32(bin, 2)

let windowString (chars: char []) = (Array.skip 1 chars) |> System.String

// taken from https://nbevans.wordpress.com/2014/03/13/really-simple-way-to-split-a-f-sequence-into-chunks-partitions/
let toChunks n (s: seq<'t>) =
    seq {
        let position = ref 0
        let buffer = Array.zeroCreate<'t> n

        for x in s do
            buffer.[position.Value] <- x

            if position.Value = n - 1 then
                yield buffer |> Array.copy
                position.Value <- 0
            else
                position.Value <- position.Value + 1

        if position.Value > 0 then
            yield Array.sub buffer 0 position.Value
    }

let parseLiteral version typeId (input: string) =
    let windowedSeq =
        input.ToCharArray() |> Array.toSeq |> toChunks 5


    let lastWindowIndex =
        windowedSeq
        |> Seq.findIndex (fun window -> window.[0] = '0')

    let windowCount = lastWindowIndex + 1
    let windows = windowedSeq |> Seq.take windowCount

    let bits =
        windows
        |> Seq.fold (fun acc window -> acc + (windowString window)) ""

    printf "windowCount %d bits %s\n" windowCount bits
    let literal = System.Convert.ToInt64(bits, 2)

    ((version, typeId, literal), input.Substring(5 * windowCount))

let parseOperator version typeId (input: string) =
    let lengthTypeId = input.[0]

    if lengthTypeId = '0' then
        // let subPacketsLength = binToDec (input.Substring(1, 15))
        ((version, typeId, (int64 -1)), input.Substring(16))
    else
        // let numberOfSubPackets = binToDec (input.Substring(1, 11))
        ((version, typeId, (int64 -2)), input.Substring(12))

let parsePacket (input: string) =
    let version = input.Substring(0, 3) |> binToDec
    let typeId = input.Substring(3, 3) |> binToDec

    let rest = input.Substring(6)

    match typeId with
    | 4 -> parseLiteral version typeId rest
    | _ -> parseOperator version typeId rest


let rec parsePackets (input: string) =
    printf "parsePackets %s\n" input

    let onlyZeros =
        input.ToCharArray()
        |> Array.forall (fun c -> c = '0')

    if input.Length > 0 && not onlyZeros then
        let (result, rest) = parsePacket input
        result :: (parsePackets rest)
    else
        []


let solve (input: string) =
    let bin =
        input.ToCharArray()
        |> Array.map hexToBin
        |> Array.reduce (+)

    let results = parsePackets bin

    let sumOfVersions =
        results
        |> List.map (fun (version, _, _) -> version)
        |> List.sum

    printf "results: %A\n" results
    printf "sumOfVersions %d\n" sumOfVersions
    sumOfVersions

solve example1
solve "38006F45291200"
solve "8A004A801A8002F478" // 16
solve "620080001611562C8802118E34" // 12
solve "C0015000016115A2E0802F182340" // 23
solve "A0016C880162017C3686B18A3D4780" // 31

// solve (Seq.head input) // 947



let rec parseOperator2 version typeId (input: string) reducer =
    let lengthTypeId = input.[0]

    if lengthTypeId = '0' then
        let subPacketsLength = binToDec (input.Substring(1, 15))

        let subPackets =
            parsePackets2 (input.Substring(16, subPacketsLength))

        let subResult =
            subPackets
            |> List.map (fun (_, _, value) -> value)
            |> List.reduce reducer

        printf "lengthTypeId 0 subResult %d\n" subResult

        ((version, typeId, subResult), input.Substring(16 + subPacketsLength))
    else
        let numberOfSubPackets = binToDec (input.Substring(1, 11))

        let subPackets =
            parseNPackets2 (input.Substring(12)) numberOfSubPackets

        let subResult =
            subPackets
            |> List.map fst
            |> List.map (fun (_, _, value) -> value)
            |> List.reduce reducer

        printf "lengthTypeId 1 subResult %d\n" subResult

        ((version, typeId, subResult), subPackets |> List.last |> snd)

and parsePacket2 (input: string) =
    let version = input.Substring(0, 3) |> binToDec
    let typeId = input.Substring(3, 3) |> binToDec

    let rest = input.Substring(6)

    match typeId with
    | 4 -> parseLiteral version typeId rest
    | 0 -> parseOperator2 version typeId rest (+)
    | 1 -> parseOperator2 version typeId rest (*)
    | 2 -> parseOperator2 version typeId rest min
    | 3 -> parseOperator2 version typeId rest max
    | 5 -> parseOperator2 version typeId rest (fun a b -> if a > b then 1 else 0)
    | 6 -> parseOperator2 version typeId rest (fun a b -> if a < b then 1 else 0)
    | 7 -> parseOperator2 version typeId rest (fun a b -> if a = b then 1 else 0)
    | _ -> parseOperator2 version typeId rest (+)


and parsePackets2 (input: string) =
    printf "parsePackets %s\n" input

    let onlyZeros =
        input.ToCharArray()
        |> Array.forall (fun c -> c = '0')

    if input.Length > 0 && not onlyZeros then
        let (result, rest) = parsePacket2 input
        result :: (parsePackets2 rest)
    else
        []

and parseNPackets2 (input: string) (n: int) =
    printf "parsePackets %s\n" input

    if n > 0 then
        let res = parsePacket2 input
        res :: (parseNPackets2 (snd res) (n - 1))
    else
        []


let solve2 (input: string) =
    let bin =
        input.ToCharArray()
        |> Array.map hexToBin
        |> Array.reduce (+)

    let results = parsePackets2 bin

    printf "results: %A\n" results


solve2 "C200B40A82" // sum of 1 and 2 -> 3
solve2 "04005AC33890" // product of 6 and 9 -> 54
solve2 "880086C3E88112" // minimum of 7, 8, 9 -> 7
solve2 "CE00C43D881120" // maximum of 7, 8, 9 -> 9
solve2 "F600BC2D8F" // 5 > 15 -> 0
solve2 "D8005AC2A8F0" // 5 < 15 -> 1
solve2 "9C005AC2F8F0" // 5 = 15 -> 0
solve2 "9C0141080250320F1802104A08" // 1 + 3 = 2 * 2 -> 1

solve2 (Seq.head input) // 660797830937
