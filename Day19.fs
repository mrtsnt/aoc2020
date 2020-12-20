module Day19

open System
open System.IO
open System.Text.RegularExpressions

type Rule = 
    | Character of string
    | Single of int list
    | Double of int list * int list

type Data = 
    { Rules : Map<int, Rule>
      Strings : string [] }

let (|Parse|_|) regex input = 
    let m = Regex(regex).Match(input)
    if m.Success then Some <| (Seq.tail >> List.ofSeq) m.Groups
    else None

let data = 
    let blocks = File.ReadAllText("data/day19").Split("\n\n")
    let rules = 
        let cpsToInts (cps : CaptureCollection) =
            cps |> Seq.map ((fun c -> c.Value) >> Int32.Parse) |> List.ofSeq
        blocks.[0].Split("\n")
        |> Array.filter (fun r -> r <> "")
        |> Array.map (fun r ->
            match r with
            | Parse """^(\d+): "(\w)"\s*$""" ms -> 
                (Int32.Parse ms.[0].Value, Character(ms.[1].Value))
            | Parse "^(\d+):(\s*\d+\s*)+\|(\s*\d+\s*)+$" ms ->
                let ruleNum = Int32.Parse ms.[0].Value 
                (ruleNum, Double(cpsToInts ms.[1].Captures, cpsToInts ms.[2].Captures))
            | Parse "^(\d+):(\s*\d+\s*)+$" ms ->
                let ruleNum = Int32.Parse ms.[0].Value 
                (ruleNum, Single(cpsToInts ms.[1].Captures))
            | r -> sprintf "can't parse: %A" r |> failwith)
    { Rules = rules |> Map.ofArray; Strings = blocks.[1].Split("\n")}

let solve1 data = 

    let rec follow rules collector = 
        match rules with
        | [x] ->
            match data.Rules.[x] with
            | Character c -> collector |> List.map (fun str -> str + c)
            | Double(fs, sn) -> [fs; sn] |> List.collect (fun rls -> follow rls collector)
            | Single rs -> follow rs collector
        | x::xs ->
            match data.Rules.[x] with
            | Character c -> follow xs (collector |> List.map (fun str -> str + c))
            | Double(fs, sn) -> follow xs ([fs; sn] |> List.collect (fun rls -> follow rls collector))
            | Single rs -> follow xs (follow rs collector)
        | [] -> sprintf "shouldn't be here - %A" rules |> failwith
    
    let (Single rls) = data.Rules.[0]
    let permutations = follow rls [""]
    data.Strings
    |> Array.filter (fun str -> List.contains str permutations)
    |> Array.length
