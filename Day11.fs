module Day11

open System.IO

type Tile = Empty | Taken | Floor

let data =
    File.ReadAllLines("data/day11")
    |> Array.map (fun l -> l |> Array.ofSeq |> Array.map (fun c ->
        match c with
        | 'L' -> Empty
        | '#' -> Taken
        | '.' -> Floor
        | c -> sprintf "unknown tile symbol %A" c |> failwith))

let countTakenAdjacent rows cols x y (grid : Tile [] []) =
    let adjacentSeats =
        let xs = [ x - 1; x; x + 1] |> List.filter (fun x -> x >= 0 && x < cols)
        let ys = [ y - 1; y; y + 1] |> List.filter (fun y -> y >= 0 && y < rows)
        xs 
        |> List.collect (fun x -> ys |> List.map (fun y -> (x, y)))
        |> List.filter (fun (a, b) -> (a, b) <> (x, y))
    adjacentSeats
    |> List.fold (fun s (x, y) -> if grid.[y].[x] = Taken then s + 1 else s) 0

let countTakenDirectional =
    let incrementBy dx dy (x, y) = (x + dx, y + dy)
    let allIncrementers = 
        [-1..1]
        |> List.collect (fun x -> [-1..1] |> List.map (fun y -> (x, y)))
        |> List.filter (fun a -> a <> (0, 0))
        |> List.map (fun (x, y) -> incrementBy x y)

    fun rows cols x y (grid : Tile [] []) ->
        let rec isDirectionTaken increment x y =
            let newX, newY = increment (x, y)
            if newX < 0 || newX >= cols || newY < 0 || newY >= rows then false
            else
                match grid.[newY].[newX] with
                | Empty -> false
                | Floor -> isDirectionTaken increment newX newY
                | Taken -> true

        allIncrementers
        |> List.map isDirectionTaken
        |> List.fold (fun s dirTaken -> if dirTaken x y then s + 1 else s) 0

let solve getTakenSeats tolerance (data : Tile [] [])  = 

    let rows = data.Length
    let cols = data.[0].Length
    let countSeats = getTakenSeats rows cols

    let allPoints = 
        [0..(cols - 1)]
        |> List.collect (fun x -> [0..(rows - 1)] |> List.map (fun y -> (x, y)))

    let deepCopy arr = arr |> Array.map Array.copy

    let isSameGrid (grid1 : Tile [] []) (grid2 : Tile [][]) =
        allPoints |> List.fold (fun s (x, y) -> s && grid1.[y].[x] = grid2.[y].[x]) true

    let rec stabilize (grid : Tile [] []) =
        let newGrid = deepCopy grid
        allPoints |> List.iter (fun (x, y) ->
            match newGrid.[y].[x] with
            | Floor -> ()
            | Empty when countSeats x y grid = 0 ->
                newGrid.[y].[x] <- Taken
            | Taken when countSeats x y grid >= tolerance ->
                newGrid.[y].[x] <- Empty
            | _ -> ())
        if isSameGrid newGrid grid then newGrid 
        else stabilize newGrid

    let finalGrid = stabilize data
    allPoints
    |> List.fold (fun s (x, y) -> if finalGrid.[y].[x] = Taken then s + 1 else s) 0

let solve1 = solve countTakenAdjacent 4
let solve2 = solve countTakenDirectional 5
