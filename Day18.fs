module Day18

open System
open System.IO
open System.Text.RegularExpressions

(*
    Part1
        Expr:
            | Primary (Op Primary)*
        Op:
            | Add
            | Multiply
        Primary:
            | Number
            | '(' Expr ')'
*)

type Token = Asterisk | Plus | Num of int64 | OpenP | CloseP
type Operator = Add | Multiply
type ExpTree =
    | Tree of ExpTree * Operator * ExpTree
    | Number of int64

let data = File.ReadAllLines("data/day18")

let (|Parse|_|) regex startAt (input : string) =
    let m = Regex(regex).Match(input.[startAt..])
    if m.Success 
    then 
        let matches = [for x in m.Groups -> x.Value]
        Some <| (List.item 1 matches, List.item 0 matches |> Seq.length)
    else None

let toTokens (row : string) =
    let rec loop (str : string) pos =
        if pos = str.Length then []
        else
            match str with
            | Parse "^\s*(\()\s*" pos (_, ln) -> OpenP::(loop str <| pos + ln)
            | Parse "^\s*(\))\s*" pos (_, ln) -> CloseP::(loop str <| pos + ln)
            | Parse "^\s*(\+)\s*" pos (_, ln) -> Plus::(loop str <| pos + ln)
            | Parse "^\s*(\*)\s*" pos (_, ln) -> Asterisk::(loop str <| pos + ln)
            | Parse "^\s*(\d+)\s*" pos (n, ln)-> 
                (Num(n |> Int64.Parse))::(loop str <| pos + ln)
            | s -> sprintf "unknown symbol %A" s.[pos..] |> failwith
    loop row 0

let rec eval ast =
    match ast with
    | Number n -> n
    | Tree(left, op, right) ->
        match op with
        | Multiply -> eval left * eval right
        | Add -> eval left + eval right

let toOperator tk =
    if tk = Plus then Add
    elif tk = Asterisk then Multiply
    else sprintf "unexpected token - %A" tk |> failwith

let toAst tks =

    let rec parseExpr tks : ExpTree * Token list =
        let primary, rem = parsePrimary tks
        continueExpr primary rem

    and continueExpr tree tks = 
        if List.isEmpty tks then tree, []
        elif tks.[0] = Asterisk || tks.[0] = Plus then 
            let op = toOperator tks.[0]
            let right, rem = parsePrimary tks.[1..]
            continueExpr (Tree(tree, op, right)) rem
        else tree, tks

    and parsePrimary tks =
        match tks.[0] with
        | Num n -> Number n, tks.[1..]
        | OpenP -> 
            let expr, rem = parseExpr tks.[1..]
            expr, rem.[1..]
        | _ -> sprintf "shouldnt be here %A" tks |> failwith

    fst <| parseExpr tks

let solve1 data =
    let calculate = (toTokens >> toAst >> eval)
    Array.sumBy calculate data

let solve2 data = 
    
    let calculate txt = 

        let rec removeAddition s =
            let addEvaluator = MatchEvaluator(fun m -> 
                let res = (m.Groups.[1].Value |> Int64.Parse) + (m.Groups.[2].Value |> Int64.Parse)
                res.ToString())
            let r = Regex.Replace(s, "\s*(\d+)\s*\+\s*(\d+)\s*", addEvaluator)
            if r = s then r else removeAddition r

        let rec removeMultiplication s =
            let addEvaluator = MatchEvaluator(fun m -> 
                let res = (m.Groups.[1].Value |> Int64.Parse) * (m.Groups.[2].Value |> Int64.Parse)
                res.ToString())
            let r = Regex.Replace(s, "\s*(\d+)\s*\*\s*(\d+)\s*", addEvaluator)
            if r = s then r else removeMultiplication r
        
        let removeParens s = Regex.Replace(s, "\s*\((\d+)\)\s*", MatchEvaluator(fun m -> m.Groups.[1].Value))

        let rec reduceInsideParens s =
            let removedAdd = removeAddition s
            let removedMult = removeMultiplication removedAdd
            let removedParens = removeParens removedMult
            if removedParens = s then removedParens
            else reduceInsideParens removedParens

        let rec reduceAll s = 
            let success, value = Int64.TryParse (s : string)
            if success then value
            else
                let iteration = Regex.Replace(s, "\([^\)\(]+\)", MatchEvaluator(fun m -> reduceInsideParens m.Groups.[0].Value))
                if iteration = s then 
                    reduceInsideParens iteration |> Int64.Parse
                else reduceAll iteration

        reduceAll txt

    Array.sumBy calculate data
