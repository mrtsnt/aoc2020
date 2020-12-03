module Day2

open System.Text.RegularExpressions
open System.IO
open System

type PasswordData = 
    { Character: char
      FirstNum: int
      SecondNum: int
      Password: string }

let policyRegex = Regex("(\d+)-(\d+)\s(\w+):\s+(\w+)")

let parsePolicy txt = 
    let m = txt |> policyRegex.Match
    { Character = m.Groups.[3].Value.[0]
      FirstNum = m.Groups.[1].Value |> Int32.Parse
      SecondNum = m.Groups.[2].Value |> Int32.Parse
      Password = m.Groups.[4].Value }

let isValid1 passwordData =
    let matches = 
        passwordData.Password
        |> Seq.groupBy id
        |> Seq.tryFind (fun gr -> (fst gr) = passwordData.Character)
    match matches with
    | Some m ->
        let len = (snd m) |> Seq.length
        len >= passwordData.FirstNum && len <= passwordData.SecondNum
    | None -> false

let isValid2 passwordData =
    let (^^) f s = 
        match (f, s) with
        | (true, true) -> false
        | (false, false) -> false
        | _ -> true
    let isMatchingChar pos = passwordData.Password.[pos - 1] = passwordData.Character
    (passwordData.FirstNum |> isMatchingChar) ^^ (passwordData.SecondNum |> isMatchingChar)

let getPolicies () =
    File.ReadAllLines("data/day2")
    |> Array.map parsePolicy
    |> List.ofArray

let solve1 policies = 
    policies
    |> List.filter isValid1
    |> List.length

let solve2 policies =
    policies
    |> List.filter isValid2
    |> List.length
