module UnitTests.MeasureSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types
open Domain.MeasureBuilder

let ``it generates events between two measures`` =
    tt
        "it generates events between two measures"
        [ { Id = "the current measure is the first measure"
            Data = Clef.G, None, emptyMeasure >> withCNaturalKeySignature >> withCommonTimeSignature <| MeasureNumber 1
            ExpectedResult =
              [ NoteName.C
                |> KeySignature
                |> MeasureEvent.DefineKeySignature

                { Numerator = 4
                  Denominator = Duration.QuarterNote }
                |> MeasureEvent.DefineTimeSignature

                Clef.G |> MeasureEvent.DefineClef ] } ]
    <| fun (initialClef, previousMeasure, currentMeasure) (expectedResult) ->
        let events = Measure.generateEvents initialClef previousMeasure currentMeasure

        events |> containsAll "The expected events were not found" expectedResult

[<Tests>]
let MeasureSpec =
    testList "MeasureSpec" [ ``it generates events between two measures`` ]
