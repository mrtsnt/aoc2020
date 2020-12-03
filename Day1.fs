module Day1

open System
open System.IO

let getData () = 
    File.ReadAllLines("data/day1")
    |> Array.map Int32.Parse
    |> List.ofArray

let solve1 data = 
    data
    |> List.collect (fun a -> data |> List.map (fun b -> (a, b)))
    |> List.find (fun (a, b) -> a + b = 2020)
    |> (fun (a, b) -> a * b)

let solve2 data =
    data
    |> List.collect (fun a -> data |> List.map (fun b -> (a, b)))
    |> List.collect (fun (a, b) -> data |> List.map (fun c -> (a, b, c)))
    |> List.find (fun (a, b, c) -> a + b + c = 2020)
    |> (fun (a, b, c) -> a * b * c)
