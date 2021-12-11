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
