let exampleInput =
    "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let lines = exampleInput.Split('\n')

let input =
    System.IO.File.ReadLines("inputs/day11.txt")
    |> Seq.toArray

let inline charToInt char = int char - int '0'

let getNeighbourhood (map: int [,]) (x: int) (y: int) =
    [ if (x - 1 >= 0) then yield (x - 1, y)
      if (x - 1 >= 0 && y - 1 >= 0) then
          yield (x - 1, y - 1)
      if (x - 1 >= 0 && y + 1 < 10) then
          yield (x - 1, y + 1)
      if (x + 1 < 10) then yield (x + 1, y)
      if (x + 1 < 10 && y - 1 >= 0) then
          yield (x + 1, y - 1)
      if (x + 1 < 10 && y + 1 < 10) then
          yield (x + 1, y + 1)
      if (y - 1 >= 0) then yield (x, y - 1)
      if (y + 1 < 10) then yield (x, y + 1) ]

let rec flash (state: int [,]) =
    let mutable flashCount = 0

    let newState =
        state
        |> Array2D.map (fun cell ->
            if cell > 9 then
                flashCount <- flashCount + 1
                System.Int32.MinValue
            else
                cell)


    if (flashCount = 0) then
        (state
         |> Array2D.map (fun cell -> if cell < 0 then 0 else cell),
         0)
    else
        for x in 0 .. Array2D.length1 state - 1 do
            for y in 0 .. Array2D.length2 state - 1 do
                if state.[x, y] > 9 then
                    (getNeighbourhood state x y)
                    |> Seq.iter (fun (u, v) -> newState.[u, v] <- newState.[u, v] + 1)

        let (recState, recFlashCount) = flash newState
        (recState, recFlashCount + flashCount)


let step (state: int [,]) =
    let energyIncrease = state |> Array2D.map (fun x -> x + 1)
    flash energyIncrease

let solve (input: array<string>) =
    let intArray =
        input
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

    let energyLevels =
        Array2D.init<int> 10 10 (fun x y -> intArray.[x].[y])

    let result =
        { 1 .. 100 }
        |> Seq.fold
            (fun (state, flashCount) _ ->
                let (newState, addFlashCount) = step state
                (newState, flashCount + addFlashCount))
            (energyLevels, 0)

    printf "result %A\n" result
    snd result


solve lines // 1656
solve input // 1739
