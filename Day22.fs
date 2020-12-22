module Day22

open System
open System.IO

let data = 
    let blocks = File.ReadAllText("data/day22").Split("\n\n")
    blocks |> Array.map (fun b ->
        b.Split("\n").[1..]
        |> Array.filter (fun r -> r <> "")
        |> Array.map Int32.Parse
        |> List.ofArray)

let calcScore ls = ls |> List.rev |> List.mapi (fun i e -> (i + 1) * e) |> List.sum

let rec solve1 p1 p2 = 
    match p1, p2 with
    | x::xs, y::ys when x > y -> solve1 (xs @ [x;y]) ys
    | x::xs, y::ys when x < y -> solve1 xs (ys @ [y;x])
    | [], ys -> calcScore ys
    | xs, [] -> calcScore xs
    | _, _ -> sprintf "shouldnt be here p1:%A, p2:%A" p1 p2 |> failwith

let solve2 p1 p2 =
    let calcScore ls = ls |> List.rev |> List.mapi (fun i e -> (i + 1) * e) |> List.sum
    let rec combat p1 p2 prev =
        if List.contains (p1, p2) prev then true, p1
        else
            match p1, p2 with
            | x::xs, y::ys when List.length xs >= x && List.length ys >= y ->
                if fst <| combat xs.[0..(x - 1)] ys.[0..(y - 1)] []
                then combat (xs @ [x;y]) ys ((p1, p2)::prev)
                else combat xs (ys @ [y;x]) ((p1, p2)::prev)
            | x::xs, y::ys when x > y -> combat (xs @ [x;y]) ys ((p1, p2)::prev)
            | x::xs, y::ys when x < y -> combat xs (ys @ [y;x]) ((p1, p2)::prev)
            | [], ys -> false, ys
            | xs, [] -> true, xs
            | _, _ -> sprintf "shouldnt be here p1:%A, p2:%A" p1 p2 |> failwith
    combat p1 p2 [] |> snd |> calcScore
