open System.Collections.Generic

let exampleInput =
    "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"


let lines = exampleInput.Split('\n')

let input =
    System.IO.File.ReadLines("inputs/day15.txt")
    |> Seq.toArray

let inline charToInt char = int char - int '0'

type Point = int * int

let getNeighbourhood (map: int [,]) ((x, y): int * int) =
    [ if (x - 1 >= 0) then yield (x - 1, y)
      if (x + 1 < (map.GetLength 0)) then
          yield (x + 1, y)
      if (y - 1 >= 0) then yield (x, y - 1)
      if (y + 1 < (map.GetLength 1)) then
          yield (x, y + 1) ]

let getVertexWithMinDistance (q: HashSet<Point>) (dist: Dictionary<Point, int>) =
    let mutable min = System.Int32.MaxValue
    let mutable minKey = (-1, -1)

    for key in q do
        if (dist.ContainsKey key && dist.[key] < min) then
            min <- dist.[key]
            minKey <- key

    minKey

let length (graph: int [,]) (u: Point) (v: Point) = graph.[(fst v), snd v]

let dijkstra (graph: int [,]) (source: Point) (target: Point) =
    let dist = new Dictionary<Point, int>()
    let prev = new Dictionary<Point, Point>()
    let q = new HashSet<Point>()

    graph
    |> Array2D.iteri (fun x y _val ->
        dist.Add((x, y), System.Int32.MaxValue)
        // prev.Add((x, y), System.Int32.MaxValue)
        q.Add((x, y)) |> ignore)

    dist.[source] <- 0

    while q.Count <> 0 do
        let u = getVertexWithMinDistance q dist
        q.Remove(u) |> ignore

        let neighbours =
            getNeighbourhood graph u
            |> Seq.filter (fun node -> q.Contains(node))

        neighbours
        |> Seq.iter (fun v ->
            let alt = dist.[u] + length graph u v

            if alt < dist.[v] then
                dist.[v] <- alt
                prev.[v] <- u)

    (dist, prev)



let solve (input: array<string>) =
    let intArray =
        input
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToInt)

    let map =
        Array2D.init<int> input.Length input.[0].Length (fun x y -> intArray.[x].[y])

    let target = (input.Length - 1, input.[0].Length - 1)
    let (dist, prev) = dijkstra map (0, 0) target

    printf "dist %A\n" dist

    printf "dist to target %d" dist.[target]


solve lines // 40
solve input // 602
