module Day5

open System
open System.IO

let data = File.ReadAllLines("data/day5")

let convertRow (row : string) =
    let sliceToNum (s: string) z o = Convert.ToInt32(s.Replace(o, "1").Replace(z, "0"), 2)
    let toSeatId (row, col) = row * 8 + col
    (sliceToNum row.[0..6] "F" "B", sliceToNum row.[7..9] "L" "R") |> toSeatId

let solve1 data = data |> Array.map convertRow |> Array.max

let solve2 data = 
    let sorted = data |> Array.map convertRow |> List.ofArray |> List.sort
    let rec find ls prev = 
        match ls with
        | [] -> failwith "end of list"
        | x::xs -> if prev + 1 = x then find xs x else x - 1
    find sorted <| sorted.[0] - 1
