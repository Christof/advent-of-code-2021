let exampleInput =
    "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

let exampleInput2 =
    "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

let exampleInput3 =
    "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

let input =
    "RT-start
bp-sq
em-bp
end-em
to-MW
to-VK
RT-bp
start-MW
to-hr
sq-AR
RT-hr
bp-to
hr-VK
st-VK
sq-end
MW-sq
to-RT
em-er
bp-hr
MW-em
st-bp
to-start
em-st
st-end
VK-sq
hr-st"

let parseLine (line: string) =
    let elements = line.Split('-')
    (elements.[0], elements.[1])

let isSmallCave (cave: string) =
    System.Char.IsLower <| cave.ToCharArray().[0]


let rec findPath (connections: array<string * string>) (pathTook: list<string>) (level: int) =
    if (level = 300) then
        printf "too far %A" pathTook
        0
    else
        let currentNode = List.head pathTook

        let possibleNextCaves =
            connections
            |> Seq.filter (fun (a, b) -> a = currentNode || b = currentNode)
            |> Seq.map (fun (a, b) -> if (a = currentNode) then b else a)
            |> Seq.filter (fun a ->
                if isSmallCave a then
                    not <| List.contains a pathTook
                else
                    true)

        printf "possible %A pathTook %A\n" possibleNextCaves pathTook

        let ends =
            possibleNextCaves
            |> Seq.filter (fun cave -> cave = "end")

        let options =
            possibleNextCaves
            |> Seq.filter (fun cave -> cave <> "end")

        (Seq.length ends)
        + (options
           |> Seq.map (fun option -> findPath connections (option :: pathTook) (level + 1))
           |> Seq.sum)


let solve (input: string) =
    let connections =
        input.Split('\n')
        |> Array.map parseLine
        |> Array.map (fun (a, b) -> if b = "start" then (b, a) else (a, b))

    printf "connections %A\n" connections

    let paths = findPath connections [ "start" ] 0


    printf "paths %d\n" paths
    paths

solve "start-a\na-end" // 1
solve exampleInput // 10
solve exampleInput2 // 19
solve exampleInput3 // 226
// solve input // 3463


let rec findPath2
    (connections: array<string * string>)
    (pathTook: list<string>)
    (alreadyVisitedOneSmallCaveTwice: bool)
    (level: int)
    =
    if (level = 300) then
        printf "too far %A" pathTook
        0
    else
        let currentNode = List.head pathTook

        let possibleNextCaves =
            connections
            |> Seq.filter (fun (a, b) -> a = currentNode || b = currentNode)
            |> Seq.map (fun (a, b) -> if (a = currentNode) then b else a)
            |> Seq.filter (fun a ->
                if isSmallCave a then
                    if (alreadyVisitedOneSmallCaveTwice) then
                        a <> "start" && not <| List.contains a pathTook
                    else
                        a <> "start"
                else
                    true)

        // printf "possible %A pathTook %A\n" possibleNextCaves pathTook

        let ends =
            possibleNextCaves
            |> Seq.filter (fun cave -> cave = "end")

        let options =
            possibleNextCaves
            |> Seq.filter (fun cave -> cave <> "end")

        ends
        |> Seq.iter (fun _ -> printf "%A %b\n" (List.rev pathTook) alreadyVisitedOneSmallCaveTwice)

        (Seq.length ends)
        + (options
           |> Seq.map (fun option ->
               let alreadyVisitedOneSmallCaveTwiceUpdated =
                   if (alreadyVisitedOneSmallCaveTwice) then
                       true
                   else
                       isSmallCave option
                       && List.contains option pathTook

               findPath2 connections (option :: pathTook) alreadyVisitedOneSmallCaveTwiceUpdated (level + 1))
           |> Seq.sum)

let solve2 (input: string) =
    let connections =
        input.Split('\n')
        |> Array.map parseLine
        |> Array.map (fun (a, b) -> if b = "start" then (b, a) else (a, b))

    printf "connections %A\n" connections

    let paths =
        findPath2 connections [ "start" ] false 0


    printf "paths %d\n" paths
    paths

// solve2 "start-a\na-end" // 1
solve2 "start-a\na-end\na-B" // 2
solve2 exampleInput // 36
solve2 exampleInput2 // 103
solve2 exampleInput3 // 3509
solve2 input // 91533
