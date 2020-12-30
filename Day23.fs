module Day23

let split n =
    let rec split' n =
        if n < 10 then [n]
        else (n % 10)::(split' <| n / 10)
    split' n |> List.rev

type Cup = 
    { Label : int
      mutable Next : Cup option }

let solve1 data =

    let cups = split data

    let pickup (ls : int list) pos = 
        let picked = 
            Seq.initInfinite (fun i -> ls.[i % ls.Length]) |> Seq.skip (pos + 1)
            |> Seq.take 3 |> List.ofSeq
        let rem = ls |> List.filter (fun c -> not <| List.contains c picked)
        picked, rem

    let findNextPos ls lbl =
        let sorted = ls |> List.sort
        let curPos = List.findIndex (fun c -> c = lbl) sorted
        let nextLbl = if curPos = 0 then List.last sorted else sorted.[curPos - 1]
        List.findIndex (fun c -> c = nextLbl) ls

    let mixCups cups times =
        let rec mixCups' cups pos n =
            if n = times then cups
            else 
                let picked, rem = pickup cups pos
                let insPos = findNextPos rem cups.[pos]
                let nextCups = List.take (insPos + 1) rem @ picked @ rem.[(insPos + 1)..]
                let nextPos = 
                    let posInNext = List.findIndex (fun c -> c = cups.[pos]) nextCups
                    if posInNext + 1 = cups.Length then 0 else posInNext + 1
                mixCups' nextCups nextPos (n + 1)

        mixCups' cups 0 0

    let state = mixCups cups 100

    Seq.initInfinite (fun i -> state.[i % state.Length])
    |> Seq.skipWhile (fun c -> c <> 1)
    |> Seq.skip 1
    |> Seq.take 8
    |> Seq.fold (fun s i -> sprintf "%s%d" s i) ""


let cupsFromArr (ns : int []) = 
    let fs = { Label = ns.[0]; Next = None }
    let mutable hd = fs
    for i in [1..(ns.Length - 1)] do
        hd.Next <- Some { Label = ns.[i]; Next = None }
        hd <- hd.Next.Value
    hd.Next <- Some fs
    fs

let remove3 (cups : Cup) =
    let mutable hd = cups

    let removed = hd.Next
    let newStart = hd
    for _ in [0..2] do hd <- hd.Next.Value
    newStart.Next <- hd.Next

    hd <- removed.Value
    for _ in [0..1] do hd <- hd.Next.Value
    hd.Next <- None

    removed.Value

let contains (cups : Cup) lbl =
    let mutable hd = cups
    while Option.isSome hd.Next && hd.Label <> lbl do
        hd <- hd.Next.Value
    hd.Label = lbl

let printCups (cups : Cup) max =
    let mutable i = 0
    let mutable hd = Some cups
    while i <= max && Option.isSome hd do
        printf " %d" hd.Value.Label
        i <- i + 1
        hd <- hd.Value.Next
    printf "\n"

let cupsToIndex (cups : Cup) n =
    let mutable idx = Array.create<Cup option> n None
    let mutable hd = cups
    for i in [0..(n - 1)] do
        idx.[i] <- Some hd
        hd <- hd.Next.Value
    Array.sortBy (fun (c : Cup option) -> c.Value.Label) idx

let insertAfter (cups : Cup) (picked : Cup) =
    let mutable hd = cups
    let next = hd.Next.Value
    hd.Next <- Some picked

    while Option.isSome hd.Next do hd <- hd.Next.Value
    hd.Next <- Some next

let rec findDest (idx : Cup option []) lbl picked = 
    match lbl with
    | 0 -> findDest idx idx.Length picked
    | dest when contains picked dest -> findDest idx (lbl - 1) picked
    | dest -> idx.[dest - 1]

let data = Array.append [|3;8;9;1;2;5;4;6;7|] [|10..1_000_000|]

let solve2 (data : int []) =

    let mutable cups, i = cupsFromArr data, 0
    let idx = cupsToIndex cups data.Length

    while i < 10_000_000 do
        if i % 100_000 = 0 then printfn "%d" i
        let picked = remove3 cups
        let dest = (findDest idx (cups.Label - 1) picked).Value
        insertAfter dest picked
        cups <- cups.Next.Value
        i <- i + 1

    while cups.Label <> 1 do cups <- cups.Next.Value
    cups

printCups (solve2 data) 8
