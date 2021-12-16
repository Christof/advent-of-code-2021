let exampleInput = "16,1,2,0,4,2,7,1,2,14"


let input =
    Seq.head
    <| System.IO.File.ReadLines("inputs/day7.txt")

let cost (positions: array<int>) (x: int) =
    positions
    |> Array.map (fun y -> abs (x - y))
    |> Array.sum

let solve (input: string) =
    let positions = input.Split(',') |> Array.map int

    let minValue = Array.min positions
    let maxValue = Array.max positions

    let minCost =
        [ minValue .. maxValue ]
        |> List.map (fun x -> cost positions x)
        |> List.min

    printf "min %d" minCost

    minCost


solve exampleInput
solve input
