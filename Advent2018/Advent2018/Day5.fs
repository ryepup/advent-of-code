namespace Advent2018

open System
open System.IO


module Day5 =

    let polymer = lazy (File.ReadAllLines("Advent2018/Advent2018/day5.txt") |> Seq.exactlyOne )

    let isSameLetter a b =
        (Char.ToLowerInvariant a) = (Char.ToLowerInvariant b)

    let hasReaction a b =
        isSameLetter a b && a <> b

    let rec react (polymer:list<char>) =
        match polymer with
            | [] -> failwith "can't handle empty"
            | [_] -> polymer
            | head::tail ->
                match react tail with
                    | [] -> [head]
                    | [x] -> [head;x]
                    | first::rest when hasReaction head first -> rest
                    | rest -> head::rest

    let reactString (polymer:string) =
        List.ofSeq polymer
        |> react
        |> Array.ofSeq
        |> System.String

    let solve1 polymer =
        reactString polymer
        |> Seq.length
