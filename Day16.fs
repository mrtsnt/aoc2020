module Day16

open System
open System.IO
open System.Text.RegularExpressions

type Rule = { Name : string; Ranges : (int64 * int64) list }
type Data = { Rules : Rule list; Ticket : int64 []; OtherTickets : int64 [] [] }

let data = 
    let blocks = File.ReadAllText("data/day16").Split("\n\n")

    let rules = 
        blocks.[0].Split("\n")
        |> List.ofArray
        |> List.map (fun r ->
            let m = Regex.Match(r, "([^:]+):\s(\d+)-(\d+) or (\d+)-(\d+)")
            let captures = [for x in m.Groups -> x.Value] |> List.skip 1
            let ranges = captures.[1..] |> List.map Int64.Parse
            { Name = captures.[0]
              Ranges = [(ranges.[0], ranges.[1]);(ranges.[2], ranges.[3])] })

    let ticket = blocks.[1].Split("\n").[1].Split(",") |> Array.map Int64.Parse

    let otherTickets = 
        blocks.[2].Split("\n").[1..]
        |> Array.filter (fun r -> r <> "")
        |> Array.map (fun r -> r.Split(",") |> Array.map Int64.Parse)
    
    { Rules = rules; Ticket = ticket; OtherTickets = otherTickets }

let getInvalidValues rules =
    let allRanges = rules |> List.collect (fun r -> r.Ranges)
    fun ticket ->
        ticket
        |> Array.fold (fun s vl ->
            let validBySome = allRanges |> List.fold (fun s (rL, rU) -> s || (vl >= rL && vl <= rU)) false
            if validBySome then s else vl::s) []
        |> Array.ofList

let solve1 data =
    let returnInvalid = getInvalidValues data.Rules
    data.OtherTickets |> Array.collect returnInvalid |> Array.sum

let solve2 data =
    let returnInvalid = getInvalidValues data.Rules
    let validTickets = data.OtherTickets |> Array.filter (returnInvalid >> Array.isEmpty)

    let getAvailablePositions r =
        let fl, fu = r.Ranges.[0]
        let sl, su = r.Ranges.[1]
        [0..(data.Ticket.Length - 1)]
        |> List.filter (fun pos ->
            validTickets 
            |> Array.forall(fun tk -> (tk.[pos] >= fl && tk.[pos] <= fu) || (tk.[pos] >= sl && tk.[pos] <= su)))

    let rec sortFields ordered rulesWithPositions =
        if List.forall (snd >> List.isEmpty) rulesWithPositions then List.rev ordered
        else
            let onlyOption = List.filter (fun r -> (List.length <| snd r) = 1) rulesWithPositions |> List.exactlyOne
            let remaining = rulesWithPositions |> List.map (fun (n, ps) -> 
                n, ps |> List.filter (fun p -> (snd >> List.head) onlyOption <> p))
            sortFields (onlyOption::ordered) remaining

    data.Rules
    |> List.map (fun r -> r.Name, getAvailablePositions r)
    |> sortFields []
    |> List.filter (fun (n, _) -> n.StartsWith("departure"))
    |> List.map (snd >> List.head)
    |> List.fold (fun s idx -> data.Ticket.[idx] * s) 1L
