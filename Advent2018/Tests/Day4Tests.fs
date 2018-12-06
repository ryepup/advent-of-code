module Day4Tests

open System
open Expecto
open Advent2018
open Expecto.Flip

let lines = [
    "[1518-11-01 00:00] Guard #10 begins shift"
    "[1518-11-01 00:05] falls asleep"
    "[1518-11-01 00:25] wakes up"
    "[1518-11-01 00:30] falls asleep"
    "[1518-11-01 00:55] wakes up"
    "[1518-11-01 23:58] Guard #99 begins shift"
    "[1518-11-02 00:40] falls asleep"
    "[1518-11-02 00:50] wakes up"
    "[1518-11-03 00:05] Guard #10 begins shift"
    "[1518-11-03 00:24] falls asleep"
    "[1518-11-03 00:29] wakes up"
    "[1518-11-04 00:02] Guard #99 begins shift"
    "[1518-11-04 00:36] falls asleep"
    "[1518-11-04 00:46] wakes up"
    "[1518-11-05 00:03] Guard #99 begins shift"
    "[1518-11-05 00:45] falls asleep"
    "[1518-11-05 00:55] wakes up"
]

[<Tests>]
let tests =
    testList "Day4" [
        testList "makeMinutesAsleep" [
            testCase "can sum one day" <| fun _ ->
                Day4.parseLogEntries lines
                |> Seq.groupBy (fun (x:Day4.LogEntry) -> x.Id)
                |> Seq.take 1
                |> Seq.collect (fun (_, entries) -> entries)
                |> Seq.groupBy (fun (x:Day4.LogEntry) -> x.Date.Date)
                |> Seq.take 1
                |> Seq.collect (fun (_, entries) -> entries)
                |> Day4.makeMinutesAsleep
                |> Seq.sumBy Convert.ToInt32
                |> Expect.equal "counts sleeping minutes" 45

            testCase "can sum multiple days" <| fun _ ->
                Day4.parseLogEntries lines
                |> Seq.filter (fun x -> x.Id = (Some (Day4.GuardId 10)))
                |> Day4.makeGuardLogs
                |> Seq.sumBy (fun x -> x.TotalAsleep)
                |> Expect.equal "counts sleeping minutes" 50

            testCase "solve1" <| fun _ ->
                Day4.parseLogEntries lines
                |> Day4.makeGuardLogs
                |> Day4.solve1
                |> Expect.equal "counts sleeping minutes" 240
        ]
    ]
