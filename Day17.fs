module Day17

open System.IO

type Cell = Active | Empty

module Array4D =
    let iteri f (arr : 'T[,,,]) =
        for x in [0..(arr.GetUpperBound 0)] do
            for y in [0..(arr.GetUpperBound 1)] do
                for z in [0..(arr.GetUpperBound 2)] do
                    for w in [0..(arr.GetUpperBound 3)] do
                        f x y z w arr.[x, y, z, w]

let data3d =
    let raw = File.ReadAllLines("data/day17")
    let initial = Array3D.create (raw.[0].Length + 2) (raw.Length + 2) 3 Empty
    raw |> Array.iteri (fun y row -> row |> Seq.iteri (fun x c ->
        match c with
        | '.' -> initial.[y + 1, x + 1, 1] <- Empty
        | '#' -> initial.[y + 1, x + 1, 1] <- Active
        | c -> sprintf "unknown char - %A" c |> failwith
        )
    )
    initial
    
let data4d =
    let raw = File.ReadAllLines("data/day17")
    let initial = Array4D.create (raw.[0].Length + 2) (raw.Length + 2) 3 3 Empty
    raw |> Array.iteri (fun y row -> row |> Seq.iteri (fun x c ->
        match c with
        | '.' -> initial.[y + 1, x + 1, 1, 1] <- Empty
        | '#' -> initial.[y + 1, x + 1, 1, 1] <- Active
        | c -> sprintf "unknown char - %A" c |> failwith
        )
    )
    initial

let solve1 data = 

    let initEmpty (arr : Cell [,,]) = 
        Array3D.create (arr.GetLength 0) (arr.GetLength 1) (arr.GetLength 2) Empty

    let copyExpand (arr : Cell [,,]) =
        let newArr = Array3D.create (arr.GetLength 0 + 2) (arr.GetLength 1 + 2) (arr.GetLength 2 + 2) Empty
        arr |> Array3D.iteri (fun x y z el -> newArr.[x + 1, y + 1, z + 1] <- el)
        newArr

    let shouldExpand (arr : Cell[,,]) =
        let mutable expand = false
        [
            arr.[0, *, *];
            arr.[*, 0, *];
            arr.[*, *, 0];
            arr.[arr.GetUpperBound 0, *, *]
            arr.[*, arr.GetUpperBound 1, *]
            arr.[*, *, arr.GetUpperBound 2]
        ] |> List.iter (fun (m : Cell[,]) -> Array2D.iteri (fun _ _ el ->
            if el = Active then expand <- true) m)
        expand

    let getActiveNeighbours (x, y, z) (arr : Cell [,,]) =
        let mutable cnt = 0
        arr.[(x - 1)..(x + 1), (y - 1)..(y + 1), (z - 1)..(z + 1)]
        |> Array3D.iteri (fun _ _ _ el -> if el = Active then cnt <- cnt + 1)
        if arr.[x, y, z] = Active then cnt - 1 else cnt


    let getActiveCount (arr : Cell [,,]) =
        let mutable cnt = 0
        Array3D.iteri (fun _ _ _ el -> if el = Active then cnt <- cnt + 1) arr
        cnt

    let rec loop arr n =
        let cp = initEmpty arr
        if n = 0 then getActiveCount arr, arr
        else
            arr |> Array3D.iteri (fun x y z e ->
                match e with
                | Active ->
                    match getActiveNeighbours (x, y, z) arr with
                    | 2 | 3 -> cp.[x, y, z] <- Active
                    | _ -> ()
                | Empty ->
                    if getActiveNeighbours (x, y, z) arr = 3 then cp.[x, y, z] <- Active
            )
            if shouldExpand cp then loop (copyExpand cp) (n - 1)
            else loop cp (n - 1)

    loop data 6

let solve2 data = 

    let initEmpty (arr : Cell [,,,]) = 
        Array4D.create (arr.GetLength 0) (arr.GetLength 1) (arr.GetLength 2) (arr.GetLength 3) Empty

    let copyExpand (arr : Cell [,,,]) =
        let newArr = Array4D.create (arr.GetLength 0 + 2) (arr.GetLength 1 + 2) (arr.GetLength 2 + 2) (arr.GetLength 3 + 2) Empty
        arr |> Array4D.iteri (fun x y z w el -> newArr.[x + 1, y + 1, z + 1, w + 1] <- el)
        newArr

    let shouldExpand (arr : Cell[,,,]) =
        let mutable expand = false
        [
            arr.[0, *, *, *]
            arr.[*, 0, *, *]
            arr.[*, *, 0, *]
            arr.[*, *, *, 0]
            arr.[arr.GetUpperBound 0, *, *, *]
            arr.[*, arr.GetUpperBound 1, *, *]
            arr.[*, *, arr.GetUpperBound 2, *]
            arr.[*, *, *, arr.GetUpperBound 3]
        ] |> List.iter (fun (m : Cell[,,]) -> Array3D.iteri (fun _ _ _ el ->
            if el = Active then expand <- true) m)
        expand

    let getActiveNeighbours (x, y, z, w) (arr : Cell [,,,]) =
        let mutable cnt = 0
        arr.[(x - 1)..(x + 1), (y - 1)..(y + 1), (z - 1)..(z + 1), (w - 1)..(w + 1)]
        |> Array4D.iteri (fun _ _ _ _ el -> if el = Active then cnt <- cnt + 1)
        if arr.[x, y, z, w] = Active then cnt - 1 else cnt


    let getActiveCount (arr : Cell [,,,]) =
        let mutable cnt = 0
        Array4D.iteri (fun _ _ _ _ el -> if el = Active then cnt <- cnt + 1) arr
        cnt

    let rec loop arr n =
        let cp = initEmpty arr
        if n = 0 then getActiveCount arr, arr
        else
            arr |> Array4D.iteri (fun x y z w e ->
                match e with
                | Active ->
                    match getActiveNeighbours (x, y, z, w) arr with
                    | 2 | 3 -> cp.[x, y, z, w] <- Active
                    | _ -> ()
                | Empty ->
                    if getActiveNeighbours (x, y, z, w) arr = 3 then cp.[x, y, z, w] <- Active
            )
            if shouldExpand cp then loop (copyExpand cp) (n - 1)
            else loop cp (n - 1)

    loop data 6
