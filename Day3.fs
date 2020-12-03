module Day3

open System.IO

let getGrid () = 
    File.ReadAllLines("data/day3")
    |> Array.map (fun row -> row |> Array.ofSeq)

let solve (grid : char [] []) xStep yStep =
    let rows = grid |> Array.length
    let cols = grid.[0] |> Array.length
    let getNextXPosition x =
        if (x + xStep) >= cols then (x + xStep) % cols else (x + xStep)
    let rec loop x y sum =
        if y >= rows then
            sum
        else
            let newX = getNextXPosition x
            let newY = y + yStep
            let newSum = sum + if grid.[y].[x] = '#' then 1 else 0
            loop newX newY newSum
    loop 0 0 0 |> int64

let solve1 grid =
    solve grid 3 1

let solve2 grid =
    [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.fold (fun sum (xStep, yStep) -> 
        sum * solve grid xStep yStep) 1L
