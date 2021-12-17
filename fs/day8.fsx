let exampleInput =
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

let exampleInput2 =
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
        .Split('\n')

let input =
    System.IO.File.ReadLines("inputs/day8.txt")

let is1Or4Or7Or8 (outputValue: string) =
    let l = outputValue.Length

    l = 2 || l = 4 || l = 3 || l = 7

let solve (input: seq<string>) =
    let outputValues =
        input
        |> Seq.map (fun line -> line.Split(" | ").[1].Split(' '))

    let countOf1Or4Or7Or8 =
        outputValues
        |> Seq.map (fun values -> Seq.filter is1Or4Or7Or8 values |> Seq.length)
        |> Seq.sum

    printf "count %d\n" countOf1Or4Or7Or8

solve exampleInput2
solve input


let includesOtherSegmentsChar (other: array<char>) (test: string) =
    other
    |> Array.forall (fun otherChar -> test.Contains(otherChar))

let includesOtherSegments (other: string) (test: string) =
    includesOtherSegmentsChar (other.ToCharArray()) test

let getSignalPattern (i: array<string>) =
    let one = i |> Array.find (fun x -> x.Length = 2)
    let four = i |> Array.find (fun x -> x.Length = 4)
    let seven = i |> Array.find (fun x -> x.Length = 3)
    let eight = i |> Array.find (fun x -> x.Length = 7)

    // length 5
    let length5 = i |> Array.where (fun x -> x.Length = 5)

    let three =
        length5
        |> Array.find (includesOtherSegments seven)

    let fourWithoutOne =
        four.ToCharArray()
        |> Array.except (one.ToCharArray())

    let five =
        length5
        |> Array.find (includesOtherSegmentsChar fourWithoutOne)

    let two = length5 |> Array.except [ three; five ]

    let length6 = i |> Array.where (fun x -> x.Length = 6)

    let nine =
        length6
        |> Array.where (includesOtherSegments seven)
        |> Array.find (includesOtherSegments four)

    let six =
        length6
        |> Array.except [ nine ]
        |> Array.find (includesOtherSegments five)

    let zero = length6 |> Array.except [ nine; six ]

    [| zero.[0]
       one
       two.[0]
       three
       four
       five
       six
       seven
       eight
       nine |]

let sort (s: string) =
    s.ToCharArray() |> Array.sort |> System.String

let solveLine (line: string) =
    let parts = line.Split(" | ")
    let patternPart = parts.[0].Split(' ')
    let outputValues = parts.[1].Split(' ')

    let patterns =
        getSignalPattern patternPart |> Array.map sort

    let decodesValues =
        outputValues
        |> Array.map sort
        |> Array.map (fun output -> patterns |> Array.findIndex (fun p -> p = output))

    let number =
        1000 * decodesValues.[0]
        + 100 * decodesValues.[1]
        + 10 * decodesValues.[2]
        + decodesValues.[3]

    number

let solve2 (input: seq<string>) =
    let numbers = input |> Seq.map solveLine
    printf "numbers %A\n" numbers

    let sum = Seq.sum numbers
    printf "sum %d\n" sum
    sum

solve2 (exampleInput2)
solve2 (input)
