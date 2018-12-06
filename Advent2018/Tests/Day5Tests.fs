module Day5Tests

open Expecto
open Advent2018
open Expecto.Flip


let hasReactionTestCase input expected =
    testCase (sprintf "%s" input) <| fun _ ->
        Day5.hasReaction input.[0] input.[1]
        |> Expect.equal "shoud react?" expected


[<Tests>]
let tests =
    testList "day5" [
        testCase "can read polymer" <| fun _ ->
            Day5.polymer.Value
            |> Seq.length
            |> Expect.equal "can read polymer" 50000
        testCase "can react" <| fun _ ->
            Day5.reactString "dabAcCaCBAcCcaDA"
            |> Expect.equal "reacts fully" "dabCBAcaDA"
        testList "hasReaction" [
            hasReactionTestCase "Aa" true
            hasReactionTestCase "bB" true
            hasReactionTestCase "AA" false
            hasReactionTestCase "ab" false
            hasReactionTestCase "aB" false
            hasReactionTestCase "Ab" false
        ]
        testCase "solve1" <| fun _ ->
            Day5.solve1 Day5.polymer.Value
            |> Expect.equal "part one solution" 10
    ]
