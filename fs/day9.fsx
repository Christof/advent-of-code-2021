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
    [ if (x - 1 >= 0) then yield (x - 1, y)
      if (x + 1 < (map.GetLength 0)) then
          yield (x + 1, y)
      if (y - 1 >= 0) then yield (x, y - 1)
      if (y + 1 < (map.GetLength 1)) then
          yield (x, y + 1) ]

let getFullNeighbourhood (map: int [,]) (x: int) (y: int) =
    seq {
        yield (x - 1, y)
        yield (x + 1, y)
        yield (x, y - 1)
        yield (x, y + 1)
    }

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
    printf "Sum of risk levels %d\n" sumOfRiskLevels

    sumOfRiskLevels


solve lines
solve input


let rec basinSize (map: int [,]) (x: int) (y: int) (basinCoordsList: list<int * int>) =
    let neighbourhood = getFullNeighbourhood map x y

    let updatedList = List.append basinCoordsList [ (x, y) ]
    Array2D.set map x y -1

    neighbourhood
    |> Seq.filter (fun (x, y) -> map.[x, y] <> -1)
    |> Seq.filter (fun point -> not <| List.contains point updatedList)
    |> Seq.filter (fun (x, y) -> map.[x, y] <> 9)
    |> Seq.map (fun (x, y) -> basinSize map x y updatedList)
    |> List.concat
    |> List.append updatedList
    |> List.distinct



let solve2 (input: array<string>) =
    let intArray =
        input
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

    let rawMap =
        Array2D.init<int> input.Length input.[0].Length (fun x y -> intArray.[x].[y])

    let map =
        Array2D.init<int> (input.Length + 2) (input.[0].Length + 2) (fun _ _ -> 9)

    Array2D.blit rawMap 0 0 map 1 1 input.Length input.[0].Length

    let lowPointsMap =
        map
        |> Array2D.mapi (fun x y value ->
            if (isLowPoint map x y) then
                (value, x, y)
            else
                (9, x, y))

    let lowPoints =
        lowPointsMap
        |> flat2Darray
        |> Seq.filter (fun (depth, x, y) -> depth < 9)

    printf "lowPoints %d\n" (Seq.length lowPoints)

    let basinSizes =
        lowPoints
        |> Seq.mapi (fun index (d, x, y) -> basinSize map x y List.empty |> Seq.length)

    let threeLargest =
        basinSizes
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.toArray

    let result =
        threeLargest.[0]
        * threeLargest.[1]
        * threeLargest.[2]

    printf "result %d\n" result
    result

// solve2 lines
solve2 input
