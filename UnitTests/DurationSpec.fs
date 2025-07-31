module UnitTests.DurationSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types

let ``returns duration equivalence`` =
    testTheory3
        "returns duration equivalence"
        [ case("1-16")
              .WithData(Duration.Whole, Duration.Sixteenth)
              .WithExpectedResult(DurationEquivalence.Multiple 16)

          case("16-16")
              .WithData(Duration.Sixteenth, Duration.Sixteenth)
              .WithExpectedResult(DurationEquivalence.Multiple 1)

          case("4-16")
              .WithData(Duration.Quarter, Duration.Sixteenth)
              .WithExpectedResult(DurationEquivalence.Multiple 4)

          case("2-4")
              .WithData(Duration.Half, Duration.Quarter)
              .WithExpectedResult(DurationEquivalence.Multiple 2)

          case("4-2")
              .WithData(Duration.Quarter, Duration.Half)
              .WithExpectedResult(DurationEquivalence.Divider 2)

          case("16-2")
              .WithData(Duration.Sixteenth, Duration.Half)
              .WithExpectedResult(DurationEquivalence.Divider 8)

          case("1-2")
              .WithData(Duration.Whole, Duration.Half)
              .WithExpectedResult(DurationEquivalence.Multiple 2) ]
    <| fun (targetDuration, unitOfEquivalence) (expectedResult) ->
        let result = Duration.getEquivalence unitOfEquivalence targetDuration
        result |> equal "The calculated equivalence is incorrect" expectedResult

[<Tests>]
let DurationSpec = testList "DurationSpec" [ ``returns duration equivalence`` ]
