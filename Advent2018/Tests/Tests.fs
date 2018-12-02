module Tests

open Expecto
open Advent2018
open Expecto.Flip

let scanTestCase name deltas expected =
    testCase name <| fun _ ->
        Day1.scan deltas
        |> Expect.equal "repeated frequency" expected

let hasRepeatsTestCase input isDouble isTriple =
    testList (sprintf "testing %s" input) [
        testCase "isDouble" <| fun _ ->
            Day2.hasDouble input |> Expect.equal "double?" isDouble
        testCase "isTriple" <| fun _ ->
            Day2.hasTriple input |> Expect.equal "triple?" isTriple
    ]


[<Tests>]
let tests =
    testList "advent" [
        testList "day1" [
            scanTestCase "simple" [1;-1] 0
            scanTestCase "still simple" [3;3;4;-2;-4] 10
            scanTestCase "requires more than one cycle" [-6;3;8;5;-6] 5
            scanTestCase "requires many cycles" [7;7;-2;-7;-4] 14
        ]
        testList "day2" [
            hasRepeatsTestCase "abcdef" false false
            hasRepeatsTestCase "bababc" true true
            hasRepeatsTestCase "abbcde" true false
            hasRepeatsTestCase "abcccd" false true
            hasRepeatsTestCase "aabcdd" true false
            hasRepeatsTestCase "abcdee" true false
            hasRepeatsTestCase "ababab" false true
            testCase "checksum" <| fun _ ->
                Day2.checksum ["abcdef"; "bababc";"abbcde";"abcccd";"aabcdd";"abcdee";"ababab"]
                |> Expect.equal "" 12
        ]

    ]
