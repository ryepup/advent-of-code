module Day6Tests

open Expecto
open Advent2018.Day6
open Expecto.Flip
open Day4Tests


let lines = [
    "1, 1" // A
    "1, 6" // B
    "8, 3" // C
    "3, 4" // D
    "5, 5" // E
    "8, 9" // F
]

[<Tests>]
let tests =
    testList "day6" [
        testCase "parsePoint" <| fun _ ->
            parsePoint lines.[1]
            |> Expect.equal "parses OK" {X=1;Y=6}
        testCase "getBounds" <| fun _ ->
            Seq.map parsePoint lines
            |> getBounds
            |> Expect.equal "find the limits" {X=8; Y=9}
        testList "manhattanDist" [
            testCase "same col" <| fun _ ->
                manhattanDist {X=1;Y=10} {X=1;Y=1}
                |> Expect.equal "calculates" 9
            testCase "same row" <| fun _ ->
                manhattanDist {X=1;Y=1} {X=9;Y=1}
                |> Expect.equal "calculates" 8
            testCase "rando" <| fun _ ->
                manhattanDist {X=5;Y=5} {X=3;Y=1}
                |> Expect.equal "calculates" 6
        ]

        testCase "findBorderPoints" <| fun _ ->
            let points = Seq.map parsePoint lines |> Seq.toList
            findBorderPoints points
            |> Expect.containsAll "Finds correct borders" (points.[5]::points.[0..2])

        testCase "render" <| fun _ ->
            let points = Seq.map parsePoint lines |> Seq.toList
            makeGrid points
            |> (render points)
            |> Expect.equal "draws a pretty grid" """
aaaaa.ccc
aAaaa.ccc
aaaddeccc
aadddeccC
..dDdeecc
bb.deEeec
bBb.eeee.
bbb.eeeff
bbb.eefff
bbb.ffffF
"""
        testCase "solve1" <| fun _ ->
            Seq.map parsePoint lines
            |> solve1
            |> Expect.equal "finds" ({X=5; Y=5}, 17)

        testCase "solve1 full data" <| fun _ ->
            Tests.skiptest "too slow for normal runs"
            points.Value
            |> solve1
            |> Expect.equal "finds" ({X=200; Y=250}, 3223)
    ]
