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

    let isSatisfied completed _ needs =
        Set.isSubset needs (Set.ofSeq completed)

    let findSatisfied completed deps  =
        deps
        |> Map.filter (isSatisfied completed)
        |> Seq.map (fun kv -> kv.Key)
        |> Seq.sort
        |> Seq.tryHead
    ;

    let findPath steps =
        let deps = buildDependencyMap steps

        let rec walk deps completed =
            deps
            |> findSatisfied completed
            |> function
                | None -> completed
                | Some(id) ->
                    walk (Map.remove id deps) (id::completed)

        walk deps List.empty
        |> Seq.rev
        |> Array.ofSeq
        |> System.String

    let duration minDuration (c:char) =
        minDuration + 1 + (int c) - (int 'A')

    type ElfWorker = {Id:char; CompletionTime:int}

    let assemble minDuration workers steps  =
        let deps = buildDependencyMap steps

        let findYoungest elves =
            elves
            |> Seq.sortBy (fun e -> e.CompletionTime)
            |> Seq.head


        let rec walk t deps completed elves =

            let finishYoungest elves =
                let elf = findYoungest elves
                printfn "[%d] Finishing %A" t elf

                walk elf.CompletionTime deps (elf.Id::completed)
                <| List.except [elf] elves

            let startWorking id =
                let elf = {
                    Id= id
                    CompletionTime = t + (duration minDuration id)
                }
                printfn "[%d] Starting %A" t elf
                walk t (Map.remove id deps) completed (elf::elves)


            match elves with
                | e when (List.length e) = workers ->
                    printfn "[%d] everyone is busy, wait" t
                    finishYoungest elves
                | _ ->
                    // start work
                    deps
                    |> findSatisfied completed
                    |> function
                        | None when (Map.isEmpty deps) ->
                            printfn "[%d] everything is done or being worked" t
                            elves |> Seq.maxBy (fun e -> e.CompletionTime)
                        | None ->
                            printfn "[%d] Nothing ready" t
                            finishYoungest elves
                        | Some(id) ->
                            printfn "[%d] Ready to start %c" t id
                            startWorking id

        (walk 0 deps List.empty List.empty).CompletionTime

    let steps =
        lazy (
            "Advent2018/Advent2018/day7.txt"
            |> File.ReadAllLines
            |> Seq.map parse
        )
