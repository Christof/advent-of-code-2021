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
    // printf "basinSize x %d y %d list %A\n" x y basinCoordsList

    if (map.[x, y] = 9) then
        // printf "\texit 9\n"
        List.empty
    else
        // printf "\tincreaseBasen\n"
        let neighbourhood = getNeighbourhood map x y

        let updatedList = List.append basinCoordsList [ (x, y) ]

        neighbourhood
        |> List.filter (fun point -> not <| List.contains point updatedList)
        |> List.map (fun (x, y) -> basinSize map x y updatedList)
        |> List.concat
        |> List.append updatedList
// |> List.distinct



let solve2 (input: array<string>) =
    let intArray =
        input
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

    let map =
        Array2D.init<int> input.Length input.[0].Length (fun x y -> intArray.[x].[y])

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
        // |> Seq.take 3
        |> Seq.mapi (fun index (d, x, y) ->
            printf "i %d d %d x %d y %d\n" index d x y

            basinSize map x y List.empty
            |> List.distinct
            |> Seq.length)

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
