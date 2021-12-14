let exampleInput =
    "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

let lines = exampleInput.Split '\n'

let input =
    System.IO.File.ReadLines("inputs/day5.txt")

type Point = int * int
type Line = Point * Point

let parsePoint (point: string) =
    let separatorIndex = point.IndexOf(",")
    let x = point.Substring(0, separatorIndex)
    let y = point.Substring(separatorIndex + 1)

    (int x, int y)

let parseLine (line: string) =
    let separatorIndex = line.IndexOf("->")
    let firstPoint = line.Substring(0, separatorIndex - 1)
    let secondPoint = line.Substring(separatorIndex + 2)

    (parsePoint firstPoint, parsePoint secondPoint)

let parseLines (input: seq<string>) = input |> Seq.map parseLine

let getPointSeq a1 a2 =
    if a1 < a2 then
        seq { a1 .. a2 }
    else
        seq { a1 .. -1 .. a2 }

let drawLine (map: int [,]) (line: Line) =
    printf "drawLine %A \n" line
    let ((x1, y1), (x2, y2)) = line

    if (x1 = x2) then
        getPointSeq y1 y2
        |> Seq.iter (fun y -> Array2D.set map y x1 (map.[y, x1] + 1))

        map
    else if (y1 = y2) then
        getPointSeq x1 x2
        |> Seq.iter (fun x -> Array2D.set map y1 x (map.[y1, x] + 1))

        map
    else
        map


let flat2Darray board =
    seq {
        for x in [ 0 .. (Array2D.length1 board) - 1 ] do
            for y in [ 0 .. (Array2D.length2 board) - 1 ] do
                yield board.[x, y]
    }

let solve (input: seq<string>) =
    let lines = parseLines input

    let allPoints =
        Seq.append (Seq.map fst lines) (Seq.map snd lines)

    let maxX = allPoints |> Seq.map fst |> Seq.max
    let maxY = allPoints |> Seq.map snd |> Seq.max
    printf "maxX %d maxY %d\n" maxX maxY

    let map =
        Array2D.zeroCreate<int> (maxY + 1) (maxX + 1)

    let finalMap =
        lines
        |> Seq.fold (fun diagram line -> drawLine diagram line) map

    printf "finalMap\n%A\n" finalMap

    let overlapPointsCount =
        flat2Darray finalMap
        |> Seq.where (fun cell -> cell >= 2)
        |> Seq.length

    printf "points %d\n" overlapPointsCount
    overlapPointsCount

solve lines
solve input

let drawLineWithDiag (map: int [,]) (line: Line) =
    printf "drawLine %A \n" line
    let ((x1, y1), (x2, y2)) = line

    if (x1 = x2) then
        getPointSeq y1 y2
        |> Seq.iter (fun y -> Array2D.set map y x1 (map.[y, x1] + 1))

        map
    else if (y1 = y2) then
        getPointSeq x1 x2
        |> Seq.iter (fun x -> Array2D.set map y1 x (map.[y1, x] + 1))

        map
    else
        let diagSeq =
            Seq.zip (getPointSeq y1 y2) (getPointSeq x1 x2)

        printf "diag Seq %A" diagSeq

        diagSeq
        |> Seq.iter (fun (y, x) -> Array2D.set map y x (map.[y, x] + 1))

        map



let solve2 (input: seq<string>) =
    let lines = parseLines input

    let allPoints =
        Seq.append (Seq.map fst lines) (Seq.map snd lines)

    let maxX = allPoints |> Seq.map fst |> Seq.max
    let maxY = allPoints |> Seq.map snd |> Seq.max
    printf "maxX %d maxY %d\n" maxX maxY

    let map =
        Array2D.zeroCreate<int> (maxY + 1) (maxX + 1)

    let finalMap =
        lines
        |> Seq.fold (fun diagram line -> drawLineWithDiag diagram line) map

    printf "finalMap\n%A\n" finalMap

    let overlapPointsCount =
        flat2Darray finalMap
        |> Seq.where (fun cell -> cell >= 2)
        |> Seq.length

    printf "points %d\n" overlapPointsCount
    overlapPointsCount

solve2 lines
solve2 input
