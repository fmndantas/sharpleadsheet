module UnitTests.DurationSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain.Types

let ``it returns duration equivalence to the minimal duration`` =
  testTheory3 "it returns duration equivalence to the minimal duration" [
    case("1.").WithData(Duration.WholeDotted).WithExpectedResult(Duration.Equivalence.Multiple 48)
    case("1").WithData(Duration.Whole).WithExpectedResult(Duration.Equivalence.Multiple 32)
    case("2.").WithData(Duration.HalfDotted).WithExpectedResult(Duration.Equivalence.Multiple 24)
    case("2").WithData(Duration.Half).WithExpectedResult(Duration.Equivalence.Multiple 16)
    case("4.").WithData(Duration.QuarterDotted).WithExpectedResult(Duration.Equivalence.Multiple 12)
    case("4").WithData(Duration.Quarter).WithExpectedResult(Duration.Equivalence.Multiple 8)
    case("8.").WithData(Duration.EighthDotted).WithExpectedResult(Duration.Equivalence.Multiple 6)
    case("8").WithData(Duration.Eighth).WithExpectedResult(Duration.Equivalence.Multiple 4)
    case("16.").WithData(Duration.SixteenthDotted).WithExpectedResult(Duration.Equivalence.Multiple 3)
    case("16").WithData(Duration.Sixteenth).WithExpectedResult(Duration.Equivalence.Multiple 2)
    case("32").WithData(Duration.ThirtySecond).WithExpectedResult(Duration.Equivalence.Multiple 1)
  ]
  <| fun targetDuration expectedResult ->
    let result = Duration.getEquivalenceToMinimalDuration targetDuration
    result |> equal "the calculated equivalence is incorrect" expectedResult

let ``it determines if a list of durations is equivalent to another list of durations`` =
  testTheory3 "it determines if a list of durations is equivalent to another list of durations" [
    caseId(1).WithData([ Duration.Whole ], [ Duration.Whole ]).WithExpectedResult true
    caseId(2).WithData([ Duration.Whole ], [ Duration.Half ]).WithExpectedResult false
    caseId(3)
      .WithData(
        [ Duration.Quarter; Duration.Eighth; Duration.Sixteenth ],
        [ Duration.ThirtySecond; Duration.QuarterDotted; Duration.ThirtySecond ]
      )
      .WithExpectedResult(true)
    caseId(4).WithData([ Duration.ThirtySecond ], [ Duration.ThirtySecond ]).WithExpectedResult true
    caseId(5).WithData([ Duration.Half ], [ Duration.Quarter; Duration.Quarter ]).WithExpectedResult true
    caseId(6)
      .WithData(
        [ Duration.Half ],
        [
          Duration.Quarter
          Duration.Eighth
          Duration.SixteenthDotted
          Duration.ThirtySecond
        ]
      )
      .WithExpectedResult
      true
  ]
  <| fun (listA, listB) expectedResult ->
    (listA, listB)
    ||> Duration.getEquivalenceBetweenLists
    |> equal "result is incorrect" expectedResult

[<Tests>]
let DurationSpec =
  testList "duration" [
    ``it returns duration equivalence to the minimal duration``
    ``it determines if a list of durations is equivalent to another list of durations``
  ]
