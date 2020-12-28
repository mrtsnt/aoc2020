module Day21

open System.IO
open System.Text.RegularExpressions

type Food =
    { Ingredients : string list
      Allergens : string list }

let data = 
    let cpsToStrings (cps : CaptureCollection) = cps |> Seq.map (fun c -> c.Value) |> List.ofSeq
    File.ReadAllText("data/day21").Split("\n")
    |> Array.filter (fun r -> r <> "")
    |> Array.map (fun r ->
        let ingredients = Regex("(?:(\w+)[^\,\)])+").Match(r).Groups.[1].Captures |> cpsToStrings
        let allergens = Regex("(?:(\w+)[\,\)]\s*)+").Match(r).Groups.[1].Captures |> cpsToStrings
        { Ingredients = ingredients; Allergens = allergens })
    |> List.ofArray

let getAllergens data = 

    let readFood map food = 
        food.Allergens
        |> List.fold (fun mp a ->
            match Map.tryFind a mp with 
            | Some st -> Map.add a (Set.intersect st <| set food.Ingredients) mp
            | None -> Map.add a (set food.Ingredients) mp) map

    let rec clean map =
        let clean' map = 
            map |> Map.fold (fun s k v ->
                if Set.count v = 1 then
                    Map.map (fun k' v' -> if k = k' then v' else Set.difference v' v) s
                else s) map
        let cleanMap = clean' map
        if cleanMap = map then map else clean cleanMap

    let rec findAllergens map = 
        let newMap = data |> List.fold readFood map |> clean
        if newMap = map then map
        else findAllergens newMap

    findAllergens Map.empty 

let solve1 data =
    let allergens = getAllergens data |> Map.toList |> List.map snd |> Set.unionMany
    data |> List.sumBy (fun (f : Food) -> Set.difference (set f.Ingredients) allergens |> Set.count)

let solve2 data =
    getAllergens data |> Map.toList |> List.sortBy fst 
    |> List.map (snd >> Set.toList >> List.exactlyOne)
    |> List.fold (fun s i -> sprintf "%s,%s" s i) ""
    |> fun s -> s.[1..]
