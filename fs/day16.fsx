let example1 = "D2FE28"


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

let parseLiteral (input: string) =
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

    let literal = binToDec bits

    (literal, input.Substring(5 * windowCount))

let parsePacket (input: string) =
    let version = input.Substring(0, 3) |> binToDec
    let typeId = input.Substring(3, 3) |> binToDec

    let rest = input.Substring(6)

    match typeId with
    | 4 -> parseLiteral rest
    | _ -> raise (System.ArgumentException("Invalid typeId"))



let solve (input: string) =
    let bin =
        input.ToCharArray()
        |> Array.map hexToBin
        |> Array.reduce (+)

    let (lit, rest) = parsePacket bin
    printf "lit %d rest %s\n" lit bin

solve example1
