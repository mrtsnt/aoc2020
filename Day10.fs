module Day10

open System
open System.IO

let data = 
    let fileData = File.ReadAllLines("data/day10") |> Array.map Int32.Parse
    Array.append [|0; Array.max fileData + 3|] fileData |> Array.sort

let solve1 data =
    data 
    |> Array.mapi (fun i e -> if i = 0 then e else data.[i] - data.[i - 1])
    |> Array.groupBy id
    |> fun gr -> 
        let getCount key = Array.find (fun a -> fst a = key) gr |> snd |> Array.length
        getCount 1 * getCount 3

let solve2 (data : int []) =

    let rec getPossibleSteps startVal p = 
        if p = data.Length then []
        else
            let diff = data.[p] - startVal
            if diff > 3 then []
            else diff::(getPossibleSteps startVal (p + 1))

    let rec countSequences seqLn seqs data =
        match data with
        | [] -> seqs
        | x::xs ->
            if x > 1 then countSequences (seqLn + 1) seqs xs
            else
                if seqLn > 0 then countSequences 0 ((seqLn + 1)::seqs) xs
                else countSequences 0 seqs xs

    let rec waysToConnectIn3 n = 
        match n with
        | 0 | 1 -> 1
        | 2 -> 2
        | _ -> waysToConnectIn3 (n - 3) + waysToConnectIn3 (n - 2) + waysToConnectIn3 (n - 1)

    data 
    |> Array.mapi (fun i e -> getPossibleSteps e (i + 1) |> List.length)
    |> List.ofArray 
    |> (countSequences 0 [])
    |> List.map (waysToConnectIn3 >> int64)
    |> List.reduce (*)
