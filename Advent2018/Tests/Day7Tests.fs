module Day7Tests

open Expecto
open Advent2018.Day7
open Expecto.Flip

let lines = [
    "Step C must be finished before step A can begin."
    "Step C must be finished before step F can begin."
    "Step A must be finished before step B can begin."
    "Step A must be finished before step D can begin."
    "Step B must be finished before step E can begin."
    "Step D must be finished before step E can begin."
    "Step F must be finished before step E can begin."
]

let durationTestCase min char expected =
    testCase (sprintf "duration %i %c" min char) <| fun _ ->
        duration min char
        |> Expect.equal "calculates correct duration" expected


[<FTests>]
let tests =
    testList "day7" [
        testCase "parse" <| fun _ ->
            parse lines.[0]
            |> Expect.equal "parses" {Id='A'; DependsOn='C'}

        testCase "findPath" <| fun _ ->
            lines
            |> Seq.map parse
            |> findPath
            |> Expect.equal "finds the right one" "CABDFE"

        testCase "findPath real data" <| fun _ ->
            steps.Value
            |> findPath
            |> Expect.equal "finds the right one" "MNOUBYITKXZFHQRJDASGCPEVWL"

        durationTestCase 60 'A' 61
        durationTestCase 10 'B' 12
        durationTestCase 0 'C' 3

        testCase "assemble" <| fun _ ->
            lines
            |> Seq.map parse
            |> assemble 0 2
            |> Expect.equal "Find the right total" 15

        testCase "assemble real data" <| fun _ ->
            steps.Value
            |> assemble 60 5
            |> Expect.equal "Find the right total" 893

    ]
