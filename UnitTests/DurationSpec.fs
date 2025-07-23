module UnitTests.DurationSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types

let ``returns duration equivalence`` =
    testTheory2
        "returns duration equivalence"
        [

          { Id = "1-16"
            Data = (Duration.WholeNote, Duration.SixteenthNote)
            ExpectedResult = DurationEquivalence.Multiple 16 }

          { Id = "16-16"
            Data = (Duration.SixteenthNote, Duration.SixteenthNote)
            ExpectedResult = DurationEquivalence.Multiple 1 }

          { Id = "4-16"
            Data = (Duration.QuarterNote, Duration.SixteenthNote)
            ExpectedResult = DurationEquivalence.Multiple 4 }

          { Id = "2-4"
            Data = (Duration.HalfNote, Duration.QuarterNote)
            ExpectedResult = DurationEquivalence.Multiple 2 }

          { Id = "4-2"
            Data = (Duration.QuarterNote, Duration.HalfNote)
            ExpectedResult = DurationEquivalence.Divider 2 }

          { Id = "16-2"
            Data = (Duration.SixteenthNote, Duration.HalfNote)
            ExpectedResult = DurationEquivalence.Divider 8 }

          { Id = "1-2"
            Data = (Duration.WholeNote, Duration.HalfNote)
            ExpectedResult = DurationEquivalence.Multiple 2 }

          ]
    <| fun (targetDuration, unitOfEquivalence) (expectedResult) ->
        let result = Duration.getEquivalence unitOfEquivalence targetDuration
        result |> equal "The calculated equivalence is incorrect" expectedResult

[<Tests>]
let DurationSpec = testList "DurationSpec" [ ``returns duration equivalence`` ]
