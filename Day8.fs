module Day8

open System
open System.IO
open System.Text.RegularExpressions

type Instruction =
    | Nop of int
    | Jump of int
    | Acc of int

let data = 
    File.ReadAllLines("data/day8")
    |> Array.map (fun (r : string) ->
        let n = Regex.Match(r, "([\+\-]\d+)").Groups.[1].Value |> Int32.Parse
        match r with
        | r when r.StartsWith("nop") -> Nop n
        | r when r.StartsWith("jmp") -> Jump n
        | r when r.StartsWith("acc") -> Acc n
        | r -> sprintf "unable to read %A" r |> failwith)

let solve1 (data : Instruction []) =
    let rec search visited acc pos =
        if List.contains pos visited then acc
        else
            match data.[pos] with
            | Nop _ -> search (pos::visited) acc (pos + 1)
            | Jump offset -> search (pos::visited) acc (pos + offset)
            | Acc n -> search (pos::visited) (acc + n) (pos + 1)
    search [] 0 0

let solve2 (data : Instruction []) =
    let permutations =
        Array.mapi (fun i e ->
            match e with
            | Nop n -> 
                let copy = Array.copy data 
                copy.[i] <- Jump n
                Some copy
            | Jump n ->
                let copy = Array.copy data 
                copy.[i] <- Nop n
                Some copy
            | _ -> None) data
        |> Array.choose id
    let rec terminates visited acc pos permutation =
        if pos = Array.length permutation then Some acc
        elif List.contains pos visited then None
        else
            match permutation.[pos] with
            | Nop _ -> terminates (pos::visited) acc (pos + 1) permutation
            | Jump offset -> terminates (pos::visited) acc (pos + offset) permutation
            | Acc n -> terminates (pos::visited) (acc + n) (pos + 1) permutation
    permutations |> Array.map (terminates [] 0 0) |> Array.choose id |> Array.item 0
