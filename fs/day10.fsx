let exampleInput =
    "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

let lines = exampleInput.Split '\n'

let input =
    System.IO.File.ReadLines("inputs/day10.txt")

let isChunkOpener (char: char) =
    char = '('
    || char = '['
    || char = '{'
    || char = '<'

let getClosingCharacterFor (char: char) =
    match char with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> raise (System.ArgumentException("Invalid character"))

let getFirstCorruptCharacter (line: string) =
    line.ToCharArray()
    |> Array.fold
        (fun (openChunks, firstCorruptChar) character ->
            if (firstCorruptChar <> ' ') then
                (openChunks, firstCorruptChar)
            else if isChunkOpener character then
                (character :: openChunks, ' ')
            else if (getClosingCharacterFor (List.head openChunks) = character) then
                (List.tail openChunks, ' ')
            else
                (openChunks, character))
        (List.empty<char>, ' ')

let getPointsFor (char: char) =
    match char with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> raise (System.ArgumentException("Invalid character"))

let solve (input: seq<string>) =
    let sum =
        input
        |> Seq.map getFirstCorruptCharacter
        |> Seq.map snd
        |> Seq.filter (fun firstCorruptChar -> firstCorruptChar <> ' ')
        |> Seq.map getPointsFor
        |> Seq.sum

    printf "points: %d\n" sum
    sum

solve lines
solve input
