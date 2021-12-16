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

    printf "min %d\n" minCost

    minCost


solve exampleInput
solve input


let cost2 (positions: array<int>) (x: int) =
    positions
    |> Array.map (fun y -> abs (x - y)) // just the distance
    |> Array.map (fun d -> (d * d + d) / 2) // use Gauss formula to calcualte fuel cost
    |> Array.sum

let solve2 (input: string) =
    let positions = input.Split(',') |> Array.map int

    let minValue = Array.min positions
    let maxValue = Array.max positions

    let minCost =
        [ minValue .. maxValue ]
        |> List.map (fun x -> cost2 positions x)
        |> List.min

    printf "min2 %d\n" minCost

solve2 exampleInput
solve2 input
