module Day15

let solve nums turn =

    let rec memGame curr turn map = seq {
        let exists = Map.tryFind curr map
        match exists with
        | Some n ->
            yield turn - n
            yield! memGame (turn - n) (turn + 1) (Map.add curr turn map) 
        | None ->
            yield 0
            yield! memGame 0 (turn + 1) (Map.add curr turn map) 
    }

    let gameHistory = nums |> List.mapi (fun i e -> (e, i + 1)) |> Map.ofList

    memGame 0 (gameHistory.Count + 1) gameHistory
    |> Seq.mapi (fun i e -> (i + gameHistory.Count + 2, e))
    |> Seq.find (fun (i, _) -> i = turn) |> snd

let nums = [1;2;16;19;18;0]
let solution1 = solve nums 2020
let solution2 = solve nums 30_000_000
