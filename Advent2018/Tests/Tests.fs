module Tests

open Expecto
open Advent2018
open Expecto.Flip

[<Tests>]
let tests =
  testList "setup" [
    testCase "can reference code in another project" <| fun _ ->
      let sut = Advent2018()
      Expect.equal "X is static" "F#" sut.X
  ]
