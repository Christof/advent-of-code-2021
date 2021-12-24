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

let lines = exampleInput.Split("\n")

let solve (input: seq<string>) =
    let mapInput =
        input
        |> Seq.takeWhile (fun line -> line.Length > 0)
        |> Seq.map (fun line ->
            let coords = line.Split(',')
            (int coords.[0], int coords.[1]))

    let maxX = mapInput |> Seq.map fst |> Seq.max
    let maxY = mapInput |> Seq.map snd |> Seq.max

    let map =
        Array2D.init<char> (maxY + 1) (maxX + 1) (fun x y -> '.')

    mapInput
    |> Seq.iter (fun (x, y) -> Array2D.set map y x '#')

    printf "%A\n" map


solve lines
