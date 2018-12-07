namespace Advent2018

open Advent2018.Utils
open System.IO

module Day7 =
    type Step = {Id:char; DependsOn:char}

    let parse line =
        match line with
            | Regex @"Step (\w) must be finished before step (\w) can begin." [Char a; Char b] ->
                {Id=b; DependsOn=a}
            | _ -> failwith "could not parse"


    let buildDependencyMap steps =
        let allIds =
            steps
            |> Seq.collect (fun s -> [s.Id;s.DependsOn])
            |> Set.ofSeq

        let unwrap d = d.DependsOn

        let deps =
            steps
            |> Seq.groupBy (fun x -> x.Id)
            |> Seq.map (fun (x,d) ->
                (x, (d |> Seq.map unwrap |> Set.ofSeq )))

        steps
        |> Seq.map (fun s -> s.Id)
        |> Set.ofSeq
        |> Set.difference allIds
        |> Seq.map (fun id -> (id, Set.empty<char>))
        |> Seq.append deps
        |> Map.ofSeq

    let findPath steps =
        let deps = buildDependencyMap steps

        let isSatisfied completed _ needs =
            Set.isSubset needs (Set.ofSeq completed)

        let rec walk deps completed =
            deps
            |> Map.filter (isSatisfied completed)
            |> Seq.map (fun kv -> kv.Key)
            |> Seq.sort
            |> Seq.tryHead
            |> function
                | None -> completed
                | Some(id) ->
                    walk (Map.remove id deps) (id::completed)


        walk deps List.empty
        |> Seq.rev
        |> Array.ofSeq
        |> System.String

    let steps =
        lazy (
            "Advent2018/Advent2018/day7.txt"
            |> File.ReadAllLines
            |> Seq.map parse
        )
