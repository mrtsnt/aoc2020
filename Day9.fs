module Day9

open System
open System.IO

let data = File.ReadAllLines("data/day9") |> Array.map Int64.Parse

let solve1 preambleLen (data : Int64 []) =
    let getPossibleSums pos = 
        let prevs = data.[(pos - preambleLen)..(pos-1)]
        prevs 
        |> Array.collect (fun a -> Array.map (fun b -> (a, b)) prevs)
        |> Array.filter (fun (a, b) -> a <> b)
        |> Array.map (fun (a, b) -> a + b)
    data.[preambleLen..(Array.length data)]
    |> Array.mapi (fun a b -> (a + preambleLen, b))
    |> Array.find (fun (a, b) -> not (Array.contains b <| getPossibleSums a))
    |> snd

let solve2 data =
    let toFind = solve1 25 data
    let findSum startP =
        let rec findSum' p sum =
            if p = data.Length then None
            else
                if sum = toFind then Some <| p - 1
                elif sum > toFind then None
                else findSum' (p + 1) (sum + data.[p])
        Option.map (fun endP -> (startP, endP)) <| findSum' startP 0L
    [0..data.Length]
    |> List.map findSum
    |> List.choose id
    |> List.find (fun (a, b) -> a <> b)
    |> fun (a, b) -> Array.min data.[a..b] + Array.max data.[a..b]
