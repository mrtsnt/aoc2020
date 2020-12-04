module Day4

open System
open System.IO
open System.Text.RegularExpressions

let data = File.ReadAllText("data/day4").Split("\n\n") |> List.ofArray

let solve1 passports =
    let requiredFields = ["byr:"; "iyr:"; "eyr:"; "hgt:"; "hcl:"; "ecl:"; "pid:"]
    let isValid (passport : string) =
        requiredFields |> List.fold (fun s f -> s && passport.Contains(f)) true
    passports 
    |> List.fold (fun s p -> if isValid p then s + 1 else s) 0

let solve2 passports =
    let tryMatch pattern txt =
        let found = Regex.Match(txt, pattern)
        printfn "trying to match %A against %A result - %A" pattern txt found.Success
        match found.Success with
        | true -> Some(found.Groups |> Seq.tail |> Seq.map (fun g -> g.Value) |> List.ofSeq)
        | _ -> None

    let validYear pattern low high passport = 
        let found = tryMatch pattern passport
        match found with
        | Some [year] -> 
            let intYear = year |> Int32.Parse
            intYear >= low && intYear <= high
        | _ -> false

    let simpleMatch pattern passport =
        let found = tryMatch pattern passport
        match found with 
        | Some _ -> true
        | _ -> false

    let validHeight pattern passport =
        let found = tryMatch pattern passport
        match found with
        | Some [height;"cm"] ->
            let heightInt = height |> Int32.Parse
            heightInt >= 150 && heightInt <= 193
        | Some [height;"in"] ->
            let heightInt = height |> Int32.Parse
            heightInt >= 59 && heightInt <= 76
        | _ -> false

    let checks = [
        validYear "byr:(\d{4})(?:\s|$)" 1920 2002
        validYear "iyr:(\d{4})(?:\s|$)" 2010 2020
        validYear "eyr:(\d{4})(?:\s|$)" 2020 2030
        validHeight "hgt:(\d{2,3})(cm|in)(?:\s|$)"
        simpleMatch "hcl:(\#[0-9a-f]{6})(?:\s|$)"
        simpleMatch "ecl:(amb|blu|brn|gry|grn|hzl|oth)(?:\s|$)"
        simpleMatch "pid:([0-9]{9})(?:\s|$)"
    ]

    let isValidPassport passport = 
        checks 
        |> List.fold (fun s f -> s && f passport) true

    passports 
    |> List.filter isValidPassport 
    |> List.length
