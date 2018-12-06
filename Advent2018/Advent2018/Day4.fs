namespace Advent2018

open System
open System.IO
open System.Text.RegularExpressions


module Day4 =

    // define "active" pattern to get fancier in match statements
    let (|Contains|_|) search input =
        match Regex.IsMatch(input, search, RegexOptions.IgnoreCase) with
            | true -> Some(true)
            | _ -> None

    let (|Regex|_|) pattern input =
        match Regex.Match(input, pattern) with
            | m when m.Success ->
                Some(List.tail [for g in m.Groups -> g.Value])
            | _ -> None

    type GuardId = GuardId of id:int
    type LogEvent = StartShift | FallAsleep | WakeUp
    type LogEntry =
        {
            Date: DateTime
            Id: GuardId option
            Event:LogEvent
            Raw:string
        }
    type GuardLog =
        {
            Id: GuardId
            Date: DateTime
            MinutesAsleep: List<bool>
            TotalAsleep: int
        }

    let parseLogEntry (line:string) =
        let date = line.[1..16] |> DateTime.Parse
        let template = {
            Id = None
            Date = date
            Raw = line
            Event = FallAsleep // this is lying, but making it an option is annoying
        }
        match line.[16..] with
            | Regex @"Guard #(\d+) begins shift" [id] ->
                {
                    template with
                        Id = Some(GuardId (Int32.Parse id))
                        Event = StartShift
                }
            | Regex "falls asleep" _ -> {template with Event = FallAsleep}
            | Regex "wakes up" _ -> {template with Event = WakeUp}
            | _ -> failwith (sprintf "could not parse %s" line)

    let fillIds sequentialLogs =
        let mutable (lastId : GuardId option) = None
        seq {
            for l in sequentialLogs ->
                match l.Event with
                    | StartShift ->
                        lastId <- l.Id
                        l
                    | _ ->
                        match lastId with
                            | Some _ -> {l with Id = lastId}
                            | None -> failwith "we expected to have a id here"
        }

    let parseLogEntries lines =
        Seq.map parseLogEntry lines
        |> Seq.sortBy (fun x -> x.Date)
        |> fillIds

    let makeMinutesAsleep logs =
        let mutable lastMinute = 0
        let mutable isAsleep = false

        Seq.filter (fun (x:LogEntry) -> (x.Date.Hour = 0)) logs
        |> Seq.map (fun x -> (x.Date.Minute, x.Event))
        // ensure a midnight wake in case we sleep through
        |> Seq.append [(60, WakeUp)]
        |> Seq.sortBy (fun (minute, _) -> minute)
        |> Seq.collect (fun (minute, event) ->
            let results = Seq.replicate (minute - lastMinute) isAsleep
            lastMinute <- minute
            match event with
                | FallAsleep -> isAsleep <- true
                | _ -> isAsleep <- false
            results)
        |> Seq.toList


    let makeGuardLogs entries =
        Seq.groupBy (fun (x:LogEntry) -> x.Id) entries
        |> Seq.collect (fun (id, perGuardLogs) ->
            match id with
                | None -> failwith "everything needs an id"
                | Some guardId ->
                    Seq.groupBy (fun (x:LogEntry) -> x.Date.Date) perGuardLogs
                    |> Seq.map (fun (date, perDayLogs) ->
                        let minuteAsleep = makeMinutesAsleep perDayLogs
                        {
                            Id = guardId
                            Date = date
                            MinutesAsleep = minuteAsleep
                            TotalAsleep = List.sumBy (fun x -> if x then 1 else 0) minuteAsleep
                        }
            )
        )

    let guardLogs =
        lazy(
        "Advent2018/Advent2018/day4.txt"
        |> File.ReadAllLines
        |> parseLogEntries
        |> makeGuardLogs
        )

    let totalAsleep logs =
        Seq.sumBy (fun log -> log.TotalAsleep) logs

    let sleepiestMinute logs =
        Seq.map (fun x -> x.MinutesAsleep) logs
        |> Seq.map (Seq.map (fun x -> if x then 1 else 0))
        |> Seq.reduce (Seq.map2 ( + ))
        |> Seq.mapi (fun i asleep -> (i, asleep))
        |> Seq.sortByDescending (fun (_,n) -> n)
        |> Seq.head

    let solve1 guardLogs =
        Seq.groupBy (fun x -> x.Id) guardLogs
        |> Seq.map (fun (id, logs) ->
            (id, totalAsleep logs, sleepiestMinute logs))
        |> Seq.sortByDescending (fun (_, totalAsleep, _) -> totalAsleep)
        |> Seq.head
        |> function
            | ((GuardId id), _, (min, _)) -> (min * id)

    let solve2 guardLogs =
        Seq.groupBy (fun x -> x.Id) guardLogs
        |> Seq.map (fun (id, logs) ->
            (id, sleepiestMinute logs))
        |> Seq.sortByDescending (fun (_, (_,n)) -> n)
        |> Seq.head
        |> function
            | ((GuardId id), (min, _)) -> (min * id)


(*

Day4.solve1 Day4.guardLogs.Value
Day4.solve2 Day4.guardLogs.Value

*)
