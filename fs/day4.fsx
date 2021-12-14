let exampleInput =
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"


let lines = exampleInput.Split '\n'

let input =
    System.IO.File.ReadLines("inputs/day4.txt")

let rec readBoards (lines: seq<string>) (boards: seq<string [,]>) =
    if Seq.isEmpty lines then
        boards
    else
        let boardLines =
            lines
            |> Seq.skip 1
            |> Seq.take 5
            |> Seq.map (fun (line: string) -> line.Trim().Replace("  ", " ").Split ' ')
            |> Seq.toArray

        let board =
            Array2D.init<string> 5 5 (fun row col -> boardLines.[row].[col])

        readBoards (lines |> Seq.skip 6) (Seq.append boards [ board ])

let markBoard (board: string [,]) (draw: string) =
    board
    |> Array2D.map (fun cell -> if draw = cell then "*" + cell else cell)

let mark (boards: seq<string [,]>) (draw: string) =
    boards
    |> Seq.map (fun board -> markBoard board draw)

let hasBoardWon (board: string [,]) =
    [ 0 .. 4 ]
    |> Seq.exists (fun row ->
        board.[row, *]
        |> Seq.forall (fun cell -> cell.StartsWith('*')))
    || [ 0 .. 4 ]
       |> Seq.exists (fun col ->
           board.[*, col]
           |> Seq.forall (fun cell -> cell.StartsWith('*')))

let rec drawUntilWinner (draws: seq<string>) (boards: seq<string [,]>) =
    let markedBoards = mark boards (Seq.head draws)
    let winningBoard = Seq.filter hasBoardWon markedBoards

    if (not <| Seq.isEmpty winningBoard) then
        (Seq.head winningBoard, Seq.head draws)
    else
        drawUntilWinner (Seq.skip 1 draws) (markedBoards)

let flat2Darray board =
    seq {
        for x in [ 0 .. (Array2D.length1 board) - 1 ] do
            for y in [ 0 .. (Array2D.length2 board) - 1 ] do
                yield board.[x, y]
    }

let calculateBoardSum (board: string [,]) =
    board
    |> Array2D.map (fun cell ->
        if cell.StartsWith('*') then
            0
        else
            int cell)
    |> flat2Darray
    |> Seq.sum


let solve (input: seq<string>) =
    let draws = (Seq.head input).Split(',')

    let boardLines = input |> Seq.skip 1
    let boards = readBoards boardLines Seq.empty

    let (winningBoard, draw) = drawUntilWinner draws boards

    let score =
        (calculateBoardSum winningBoard) * (int draw)

    printf "score %d" score

    score


solve lines

solve input


let rec drawUntilLastWinner (draws: seq<string>) (boards: seq<string [,]>) =
    let markedBoards = mark boards (Seq.head draws)
    let winningBoard = Seq.filter hasBoardWon markedBoards

    let remainingBoards =
        Seq.filter (fun board -> not <| hasBoardWon board) markedBoards

    printf "remaining draws %d\n" <| Seq.length draws

    if (Seq.isEmpty draws || Seq.isEmpty boards) then
        raise (System.ArgumentException("Draws or boards where empty before last winner"))
    else

    if (Seq.isEmpty remainingBoards) then
        (Seq.head winningBoard, Seq.head draws)
    else
        drawUntilLastWinner (Seq.skip 1 draws) (remainingBoards)

let solve2 (input: seq<string>) =
    let draws = (Seq.head input).Split(',')

    let boardLines = input |> Seq.skip 1
    let boards = readBoards boardLines Seq.empty

    let (lastBoard, draw) = drawUntilLastWinner draws boards

    let score =
        (calculateBoardSum lastBoard) * (int draw)

    printf "score %d" score

    score

solve2 lines

solve2 input
