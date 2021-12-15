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

let solve (input: string) =
    let fish =
        input.Split(',') |> Seq.map int |> Seq.toList

    printf "fish %A\n" <| Seq.toList fish

    let finalState = simulateDays fish 80 0
    printf "final %A\n" <| Seq.toList finalState
    let sum = Seq.length finalState

    printf "sum %d\n" sum



solve exampleInput
solve input
