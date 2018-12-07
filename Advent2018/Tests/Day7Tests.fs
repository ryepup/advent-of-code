module Day7Tests

open Expecto
open Advent2018.Day7
open Expecto.Flip
open Day4Tests

let lines = [
    "Step C must be finished before step A can begin."
    "Step C must be finished before step F can begin."
    "Step A must be finished before step B can begin."
    "Step A must be finished before step D can begin."
    "Step B must be finished before step E can begin."
    "Step D must be finished before step E can begin."
    "Step F must be finished before step E can begin."
]

[<FTests>]
let tests =
    testList "day6" [
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

    ]
