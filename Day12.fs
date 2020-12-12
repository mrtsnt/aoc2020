module Day12

open System
open System.IO

type Direction = 
    | East of int 
    | West of int 
    | North of int 
    | South of int

type Rotation = 
    | Left of int 
    | Right of int

type AllMoves = 
    | DirectionalMove of Direction
    | RotationalMove of Rotation
    | ForwardMove of int

let data =
    File.ReadAllLines("data/day12")
    |> Array.map (fun l ->
        let n = l.[1..] |> Int32.Parse
        match l.[0] with 
        | 'F' -> ForwardMove n
        | 'R' -> RotationalMove (Right n)
        | 'L' -> RotationalMove (Left n)
        | 'E' -> DirectionalMove (East n)
        | 'W' -> DirectionalMove (West n)
        | 'N' -> DirectionalMove (North n)
        | 'S' -> DirectionalMove (South n)
        | _ -> failwith "unknow move")
    |> List.ofArray

let moveSimple (x, y) m =
    match m with 
    | East n -> (x + n, y)
    | West n -> (x - n, y)
    | North n -> (x, y + n)
    | South n -> (x, y - n)

let solve1 data =

    let rotateShip m curr =
        match m with
        | Left d -> 
            let degrees = curr - d
            if degrees >= 0 then degrees
            else degrees + 360
        | Right d -> abs <| (curr + d) % 360

    let moveInDirection n deg (x, y) =
        match deg with
        | 0 -> moveSimple (x, y) (North n)
        | 90 -> moveSimple (x, y) (East n)
        | 180 -> moveSimple (x, y) (South n)
        | 270 -> moveSimple (x, y) (West n)
        | _ -> failwith "invalid rotation state"

    let rec executeInstruction (deg, coords) m  =
        match m with
        | DirectionalMove dm -> (deg, moveSimple coords dm)
        | RotationalMove rm -> (rotateShip rm deg, coords)
        | ForwardMove n -> (deg, moveInDirection n deg coords)

    data 
    |> List.fold executeInstruction (90, (0, 0)) 
    |> snd 
    |> (fun (x, y) -> abs x + abs y)

let solve2 data = 

    let moveToWaypoint (wx, wy) (sx, sy) n = (sx + wx * n, sy + wy * n)

    let rotateWaypoint coords m =
        let rec rotateRight (x, y) n =
            if n = 0 then (x, y)
            else rotateRight (y, -x) <| n - 1

        match m with 
        | Right deg -> rotateRight coords <| deg / 90
        | Left deg -> rotateRight coords <| 4 - deg / 90

    let executeInstruction (wCoords, sCoords) m =
        match m with
        | DirectionalMove dm -> ((moveSimple wCoords dm), sCoords)
        | RotationalMove rm -> ((rotateWaypoint wCoords rm), sCoords)
        | ForwardMove n -> (wCoords, moveToWaypoint wCoords sCoords n)

    data 
    |> List.fold executeInstruction ((10, 1), (0, 0))
    |> snd 
    |> (fun (x, y) -> abs x + abs y)
