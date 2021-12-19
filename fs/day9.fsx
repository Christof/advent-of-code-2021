let exampleInput =
    "2199943210
3987894921
9856789892
8767896789
9899965678"


let lines = exampleInput.Split '\n'

let input =
    System.IO.File.ReadLines("inputs/day9.txt")
    |> Seq.toArray

let inline charToInt char = int char - int '0'

let getNeighbourhood (map: int [,]) (x: int) (y: int) =
    let invalid = (-1, -1)

    [ if (x - 1 >= 0) then
          (x - 1, y)
      else
          invalid
      if (x + 1 < (map.GetLength 0)) then
          (x + 1, y)
      else
          invalid
      if (y - 1 >= 0) then
          (x, y - 1)
      else
          invalid
      if (y + 1 < (map.GetLength 1)) then
          (x, y + 1)
      else
          invalid ]
    |> List.filter (fun (x, _) -> x >= 0)


let isLowPoint (map: int [,]) (x: int) (y: int) =
    let value = map.[x, y]

    getNeighbourhood map x y
    |> List.forall (fun (x, y) -> map.[x, y] > value)


let flat2Darray array2D =
    seq {
        for x in [ 0 .. (Array2D.length1 array2D) - 1 ] do
            for y in [ 0 .. (Array2D.length2 array2D) - 1 ] do
                yield array2D.[x, y]
    }

let solve (input: array<string>) =
    let intArray =
        input
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

    let map =
        Array2D.init<int> input.Length input.[0].Length (fun x y -> intArray.[x].[y])

    let riskLevels =
        map
        |> Array2D.mapi (fun x y value ->
            if (isLowPoint map x y) then
                value + 1
            else
                0)

    let sumOfRiskLevels = riskLevels |> flat2Darray |> Seq.sum
    printf "Sum of risk levels %d" sumOfRiskLevels

    sumOfRiskLevels


solve lines
solve input
