let exampleInput =
    "forward 5
down 5
forward 8
up 3
down 8
forward 2"

let lines = exampleInput.Split '\n'

let input = System.IO.File.ReadLines("fs/day2.txt")

let (|Command|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length) |> int)
    else
        None

let reducer ((x, y): int * int) (line: string) =
    match line with
    | Command "forward" amount -> (x + amount, y)
    | Command "up" amount -> (x, y - amount)
    | Command "down" amount -> (x, y + amount)
    | _ -> (x, y)

let solve (input: seq<string>) =
    let (x, y) = input |> Seq.fold reducer (0, 0)

    printf "result is %d" (x * y)


let reducer2 ((x, y, aim): int * int * int) (line: string) =
    match line with
    | Command "forward" amount -> (x + amount, y + aim * amount, aim)
    | Command "up" amount -> (x, y, aim - amount)
    | Command "down" amount -> (x, y, aim + amount)
    | _ -> (x, y, aim)

let solve2 (input: seq<string>) =
    let (x, y, _) = input |> Seq.fold reducer2 (0, 0, 0)

    printf "result is %d horizontal position %d depth %d" (x * y) x y
