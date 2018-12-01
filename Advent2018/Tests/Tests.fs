module Tests

open Expecto
open Advent2018
open Expecto.Flip

let scanTestCase name deltas expected =
    testCase name <| fun _ ->
        // WTF do I need to do so vscode+ionide will recognize `Day1` ?!
        Day1.scan deltas
        |> Expect.equal "repeated frequency" expected

[<Tests>]
let tests =
  testList "advent2018" [
    testCase "can reference code in another project" <| fun _ ->
      let sut = Advent2018()
      sut.X |> Expect.equal "X is static" "F#"
    scanTestCase "simple" [1;-1] 0
    scanTestCase "still simple" [3;3;4;-2;-4] 10
    scanTestCase "requires more than one cycle" [-6;3;8;5;-6] 5
    scanTestCase "requires many cycles" [7;7;-2;-7;-4] 14
  ]
