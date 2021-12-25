let exampleInput =
    "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

let input =
    System.IO.File.ReadLines("inputs/day13.txt")

let lines = exampleInput.Split("\n")

let foldY (map: char [,]) (y: int) =
    let l1 = Array2D.length1 map
    let l1New = l1 / 2
    let l2 = Array2D.length2 map

    Array2D.init<char> l1New l2 (fun x y ->

        if (map.[x, y] = '#' || map.[l1 - x - 1, y] = '#') then
            '#'
        else
            '.')

let foldX (map: char [,]) (x: int) =
    let l1 = Array2D.length1 map
    let l2 = Array2D.length2 map
    let l2New = l2 / 2

    Array2D.init<char> l1 l2New (fun x y ->

        if (map.[x, y] = '#' || map.[x, l2 - y - 1] = '#') then
            '#'
        else
            '.')

let solve (input: seq<string>) =
    let mapInput =
        input
        |> Seq.takeWhile (fun line -> line.Length > 0)
        |> Seq.map (fun line ->
            let coords = line.Split(',')
            (int coords.[0], int coords.[1]))

    let maxX = mapInput |> Seq.map fst |> Seq.max
    let maxY = mapInput |> Seq.map snd |> Seq.max

    let folds =
        input
        |> Seq.skipWhile (fun line -> line.Length > 0)
        |> Seq.skip 1

    let map =
        Array2D.init<char> (maxY + 1) (maxX + 1) (fun x y -> '.')

    mapInput
    |> Seq.iter (fun (x, y) -> Array2D.set map y x '#')

    let folded =
        folds
        |> Seq.take 1
        |> Seq.fold
            (fun m fold ->
                if fold.Contains('x') then
                    foldX m 0
                else
                    foldY m 0)
            map

    let mutable dotCount = 0

    folded
    |> Array2D.iter (fun value ->
        if value = '#' then
            dotCount <- dotCount + 1)

    printf "%d\n" dotCount
    dotCount


solve lines // 17
solve input // 759

let print (map: char [,]) =
    for x in [ 0 .. (Array2D.length1 map) - 1 ] do
        printf "\n"

        for y in [ 0 .. (Array2D.length2 map) - 1 ] do
            printf
                "%c"
                (if map.[x, y] = '#' then
                     '\u2588'
                 else
                     '.')

let getMaxCoordFromFold (foldLine: string) =
    let foldNumber =
        int
        <| foldLine.Substring(foldLine.IndexOf('=') + 1)

    2 * foldNumber

let solve2 (input: seq<string>) =
    let mapInput =
        input
        |> Seq.takeWhile (fun line -> line.Length > 0)
        |> Seq.map (fun line ->
            let coords = line.Split(',')
            (int coords.[0], int coords.[1]))


    let folds =
        input
        |> Seq.skipWhile (fun line -> line.Length > 0)
        |> Seq.skip 1

    let firstXFold =
        folds |> Seq.find (fun line -> line.Contains 'x')

    let maxX = getMaxCoordFromFold firstXFold

    let firstYFold =
        folds |> Seq.find (fun line -> line.Contains 'y')

    let maxY = getMaxCoordFromFold firstYFold

    let map =
        Array2D.init<char> (maxY + 1) (maxX + 1) (fun x y -> '.')

    mapInput
    |> Seq.iter (fun (x, y) -> Array2D.set map y x '#')

    let folded =
        folds
        |> Seq.fold
            (fun m fold ->
                if fold.Contains('x') then
                    foldX m 0
                else
                    foldY m 0)
            map

    print folded

solve2 lines
solve2 input // HECRZKPR
