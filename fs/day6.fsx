let exampleInput = "3,4,3,1,2"


let input =
    Seq.head
    <| System.IO.File.ReadLines("inputs/day6.txt")

let newState (state: int) = if state = 0 then 6 else state - 1

let simulateDay (states: list<int>) =
    let newFish =
        states
        |> List.where (fun state -> state = 0)
        |> List.map (fun _ -> 8)

    List.append (states |> List.map newState) newFish

let rec simulateDays (state: list<int>) (remainingDays: int) (day: int) =
    if (remainingDays = 0) then
        state
    else
        let newState = simulateDay state

        simulateDays newState (remainingDays - 1) (day + 1)

let rec fishSum (state: int) (remainingDays: int) =
    let days = remainingDays - 1

    if (remainingDays <= 0) then
        1
    else if state = 0 then
        (fishSum 0 (days - 6)) + (fishSum 0 (days - 8))
    else
        fishSum (state - 1) days

let solve (input: string) =
    let fish =
        input.Split(',') |> Seq.map int |> Seq.toList

    printf "fish %A\n" <| Seq.toList fish

    let sum =
        fish
        |> List.map (fun state -> fishSum state 80)
        |> List.sum

    printf "sum %d\n" sum



solve exampleInput
solve input


let solve2 (input: string) =
    let fish =
        input.Split(',') |> Seq.map int |> Seq.toList

    printf "fish %A\n" <| Seq.toList fish

    let sum =
        fish
        |> List.map (fun state -> fishSum state 256)
        |> List.sum

    printf "sum %d\n" sum

// solve2 exampleInput
