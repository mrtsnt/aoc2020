module Day13

open System
open System.IO

let data = 
    let raw = File.ReadAllLines("data/day13")
    let busIds = 
        raw.[1].Split(',')
        |> Array.map (fun a -> if a = "x" then None else Some <| Int64.Parse a)
    (Int64.Parse raw.[0], busIds)

let solve1 data =
    snd data
    |> Array.choose id
    |> Array.map (fun busId -> (busId, busId - (fst data % busId)))
    |> Array.minBy snd
    |> fun (a, b) -> a * b

let solve2 data =
    let solvePair (fr, fd) (sr, sd) =
        let rec solve' (fr, fd) (sr, sd) n =
            if (fr + fd * n) % sd = sr then fr + fd * n
            else solve' (fr, fd) (sr, sd) <| n + 1L
        (solve' (fr, fd) (sr, sd) 1L, fd * sd)

    data
    |> snd
    |> Array.mapi (fun i e -> (i, e))
    |> Array.filter (snd >> Option.isSome)
    |> Array.map (fun (idx, dep) ->
        let depVal = Option.defaultValue 0L dep
        let remainder = (depVal - int64 idx) % depVal
        ((if remainder < 0L then remainder + depVal else remainder), depVal))
    |> Array.fold solvePair (1L, 1L)
    |> fst
