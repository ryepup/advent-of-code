namespace Advent2018

open System
open System.IO


module Day5 =

    let polymer = lazy (File.ReadAllLines("Advent2018/Advent2018/day5.txt") |> Seq.exactlyOne )

    let isSameLetter a b =
        (Char.ToLowerInvariant a) = (Char.ToLowerInvariant b)

    let hasReaction a b =
        isSameLetter a b && a <> b

    let react (polymer:seq<char>) =
        Seq.rev polymer
        |> Seq.fold (fun acc char ->
            match acc with
                | [] -> [char]
                | last::rest when hasReaction last char -> rest
                | _ -> char :: acc
        ) List.empty<char>

    let reactString (polymer:string) =
        react polymer
        |> Array.ofSeq
        |> System.String

    let solve1 polymer =
        reactString polymer
        |> Seq.length
