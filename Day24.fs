module Day24

open System.IO

type Direction = E | SE | SW | W | NW | NE

let data = 
    let rec getDirections acc r=
        match r with 
        | "" -> List.rev acc
        | r when r.StartsWith("e") -> getDirections (E::acc) r.[1..]
        | r when r.StartsWith("se") -> getDirections (SE::acc) r.[2..]
        | r when r.StartsWith("sw") -> getDirections (SW::acc) r.[2..]
        | r when r.StartsWith("w") -> getDirections (W::acc) r.[1..]
        | r when r.StartsWith("nw") -> getDirections (NW::acc) r.[2..]
        | r when r.StartsWith("ne") -> getDirections (NE::acc) r.[2..]
        | r -> sprintf "can't parse %s" r |> failwith

    File.ReadAllText("data/day24").Split('\n')
    |> Array.filter (fun r -> r <> "")
    |> Array.map (getDirections [])
    |> List.ofArray

let getDestination (moves : Direction list) =
    let rec getDestination' moves (x, y, z) = 
        match moves with
        | [] -> (x, y, z)
        | d::ds ->
            match d with
            | E -> getDestination' ds (x + 1, y - 1, z)
            | SE -> getDestination' ds (x, y - 1, z + 1)
            | SW -> getDestination' ds (x - 1, y, z + 1)
            | W -> getDestination' ds (x - 1, y + 1, z)
            | NW -> getDestination' ds (x, y + 1, z - 1)
            | NE -> getDestination' ds (x + 1, y, z - 1)
    getDestination' moves (0, 0, 0)
    
let solve1 data =
    data
    |> List.map getDestination
    |> List.groupBy id
    |> List.filter (fun (_, gr) -> List.length gr % 2 = 1)
    |> List.length

let solve2 data = 

    let initialCoords = 
        data
        |> List.map getDestination
        |> List.groupBy id
        |> List.filter (fun (_, gr) -> List.length gr &&& 1 = 1)
        |> List.map fst

    let getNeighbours (x, y, z) =
        [(x + 1, y - 1, z); (x + 1, y, z - 1); (x, y + 1, z - 1);
         (x - 1, y + 1, z); (x - 1, y, z + 1); (x, y - 1, z + 1)]
        |> Set.ofList

    let passDay blackTiles =

        let allRelevantTiles = 
            blackTiles 
            |> Seq.map getNeighbours 
            |> Set.unionMany
            |> Set.union blackTiles

        allRelevantTiles |> Seq.map (fun coords ->
            let blackNeighbours = 
                getNeighbours coords
                |> Set.filter (fun co -> Set.contains co blackTiles) 
                |> Set.count
            if Set.contains coords blackTiles then
                if blackNeighbours = 1 || blackNeighbours = 2 then (coords, true)
                else (coords, false)
            else 
                if blackNeighbours = 2 then (coords, true) else (coords, false))
        |> Seq.filter snd
        |> Seq.map fst
        |> Set.ofSeq

    let rec passDays blackTiles n =
        if n = 100 then blackTiles |> Set.count
        else passDays (passDay blackTiles) (n + 1)

    passDays (initialCoords |> Set.ofList) 0
