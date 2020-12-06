module Day6

open System.Text.RegularExpressions
open System.IO

let data = File.ReadAllText("data/day6").Split("\n\n") |> List.ofArray

let solve1 data =
    data
    |> List.map (fun s -> Regex.Replace(s, "\s", "") |> Seq.groupBy id |> Seq.length)
    |> List.reduce (+)

let solve2 data =
    data
    |> List.map (fun (s : string) ->
        let totalPeople = s.Trim().Split('\n') |> Array.length
        s.Trim()
        |> Seq.groupBy id 
        |> Seq.filter (fun sq -> ((snd sq) |> Seq.length = totalPeople))
        |> Seq.length)
    |> List.reduce (+)
