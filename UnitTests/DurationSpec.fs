module UnitTests.DurationSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain.Types

let ``returns duration equivalence to the minimal duration`` =
  testTheory3 "returns duration equivalence to the minimal duration" [
    case("1").WithData(Duration.Whole).WithExpectedResult(Duration.Equivalence.Multiple 32)
    case("1.").WithData(Duration.WholeDotted).WithExpectedResult(Duration.Equivalence.Multiple 48)
    case("16").WithData(Duration.Sixteenth).WithExpectedResult(Duration.Equivalence.Multiple 2)
    case("4").WithData(Duration.Quarter).WithExpectedResult(Duration.Equivalence.Multiple 8)
    case("2").WithData(Duration.Half).WithExpectedResult(Duration.Equivalence.Multiple 16)
    case("2.").WithData(Duration.HalfDotted).WithExpectedResult(Duration.Equivalence.Multiple 24)
    case("32").WithData(Duration.ThirtySecond).WithExpectedResult(Duration.Equivalence.Multiple 1)
  ]
  <| fun targetDuration expectedResult ->
    let result = Duration.getEquivalenceToMinimalDuration targetDuration
    result |> equal "the calculated equivalence is incorrect" expectedResult

[<Tests>]
let DurationSpec =
  testList "duration" [ ``returns duration equivalence to the minimal duration`` ]
