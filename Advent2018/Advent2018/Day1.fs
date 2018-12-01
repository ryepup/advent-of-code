namespace Advent2018

module Day1 =
    open System.IO

    let frequencyChanges =
        "Advent2018/Advent2018/day1-frequency.txt"
        |> File.ReadAllLines
        |> Array.map System.Int32.Parse
        |> List.ofArray

    let day1 = List.sum frequencyChanges

    let rec scan' deltas frequencies originalDeltas cycle current =
        match deltas with
            | [] ->
                printfn "Cycling back around %i, candidates %i" cycle (Set.count frequencies)
                scan' originalDeltas frequencies originalDeltas (1+cycle) current
            | delta :: rest ->
                let newestFrequency = current + delta
                match Set.contains newestFrequency frequencies with
                    | true -> newestFrequency
                    | false -> scan' rest (Set.add newestFrequency frequencies) originalDeltas cycle newestFrequency

    let scan deltas =
        scan' deltas (Set.empty.Add(0)) deltas 0 0
