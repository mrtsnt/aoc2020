module Day14

open System
open System.IO
open System.Text.RegularExpressions

type Instruction = Mask of string | Write of uint64 * uint64

let (|Parse|_|) regex input = 
    let m = Regex(regex).Match(input)
    if m.Success then Some <| List.tail [for x in m.Groups -> x.Value]
    else None

let data = 
    File.ReadAllLines("data/day14")
    |> Array.map (fun r ->
        match r with
        | Parse @"mask\s=\s([01X]+)" [m] -> Mask m
        | Parse @"mem\[(\d+)\]\s=\s(\d+)" [adr; vl] -> 
            Write (UInt64.Parse adr, UInt64.Parse vl)
        | _ -> failwith "unknow value")
    |> List.ofArray


let solve1 data =

    let applyBitmask n (mask : string) = 
        let flipped36bits = [0..35] |> List.sumBy (fun p -> pown 2UL p)
        let rec apply n pos = 
            if pos >= mask.Length then n
            else
                match mask.[35 - pos] with
                | '1' -> apply (n ||| (1UL <<< pos)) (pos + 1)
                | '0' -> apply (n &&& ~~~(1UL <<< pos) &&& flipped36bits) (pos + 1)
                | 'X' -> apply n (pos + 1)
                | c -> sprintf "unknow char - %A" c |> failwith
        apply n 0

    let rec execute data mask map =
        match data with
        | [] -> map |> Map.toList |> List.sumBy snd
        | x::xs ->
            match x with
            | Write (adr, value) -> 
                execute xs mask (Map.add adr (applyBitmask value mask) map)
            | Mask m -> execute xs m map

    execute data "" Map.empty

let solve2 data = 

    let applyBitmask n (mask: string) =
        let flipped36bits = [0..35] |> List.sumBy (fun p -> pown 2UL p)
        let rec apply n res pos =
            if pos >= mask.Length then res
            else
                match mask.[35 - pos] with
                | '0' -> 
                    let newResults = 
                        if n &&& (1UL <<< pos) > 0UL
                        then res |> List.map (fun r -> r ||| (1UL <<< pos))
                        else res
                    apply n newResults (pos + 1)
                | '1' -> apply n (res |> List.map (fun r -> r ||| (1UL <<< pos))) (pos + 1)
                | 'X' -> 
                    let doubleResults = 
                        res 
                        |> List.collect (fun r -> 
                            [ r ||| (1UL <<< pos); r &&& ~~~(1UL <<< pos) &&& flipped36bits])
                    apply n doubleResults (pos + 1)
                | c -> sprintf "unknow char - %A" c |> failwith
        apply n [0UL] 0

    let rec execute data mask map =
        match data with
        | [] -> map |> Map.toList |> List.sumBy snd
        | x::xs -> 
            match x with
            | Mask m -> execute xs m map
            | Write (adr, value) ->
                let adrs = applyBitmask adr mask
                execute xs mask (adrs |> List.fold (fun m a -> Map.add a value m) map)

    execute data "" Map.empty
