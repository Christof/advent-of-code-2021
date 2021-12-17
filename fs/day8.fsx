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
