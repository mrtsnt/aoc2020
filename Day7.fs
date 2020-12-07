module Day7

open System
open System.Text.RegularExpressions
open System.IO

let data =
    let parseRow row =
        let topColor = Regex.Match(row, @"^(\w+\s+\w+) bags").Groups.[1].Value
        let childColors = 
            Regex.Matches(row, @"(\d+)\s(\w+\s\w+)\sbags?[\,|\.]")
            |> Seq.map (fun m -> (m.Groups.[1].Value |> Int32.Parse, m.Groups.[2].Value))
            |> List.ofSeq
        (topColor, childColors)
    File.ReadAllLines("data/day7") |> Array.map parseRow |> List.ofArray

let solve1 data =
    let rec search rule =
        let children = snd rule |> List.map snd
        match children with
        | [] -> false
        | xs when xs |> List.contains "shiny gold" -> true
        | xs -> 
            xs 
            |> List.map (fun c -> List.find (fun c' -> fst c' = c) data)
            |> List.fold (fun s r -> s || search r) false
    data |> List.filter search |> List.length

let solve2 data =
    let bagMap = data |> Map.ofList
    let children = Map.find "shiny gold" bagMap
    let rec getCount bags =
        bags
        |> List.fold (fun s b -> 
            let bagCount = fst b
            let childColors = Map.find (snd b) bagMap
            s + bagCount + bagCount * (getCount childColors)) 0
    children |> getCount
