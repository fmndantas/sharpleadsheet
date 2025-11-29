module UnitTests.ResultSpec

open Expecto
open Expecto.Flip.Expect

open Case


let traverse =
  testTheory3 "traverse" [
    case("all ok").WithData([ Ok 1; Ok 2; Ok 3 ]).WithExpectedResult(Ok [ 1; 2; 3 ])

    case("one error").WithData([ Ok 1; Error [ "e1" ]; Ok 3 ]).WithExpectedResult(Error [ "e1" ])

    case("multiple errors.a")
      .WithData([ Error [ "e1" ]; Ok 2; Error [ "e2" ]; Error [ "e3" ] ])
      .WithExpectedResult(Error [ "e1"; "e2"; "e3" ])

    case("multiple errors.b")
      .WithData([ Error [ "e1"; "e2" ]; Error [ "e4"; "e3" ] ])
      .WithExpectedResult(Error [ "e1"; "e2"; "e4"; "e3" ])
  ]
  <| fun rs expectedResult ->
    let result = Domain.Result.traverse rs
    result |> equal "should be equal" expectedResult

[<Tests>]
let tests = testList "result" [ traverse ]
