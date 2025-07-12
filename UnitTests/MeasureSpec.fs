module UnitTests.MeasureSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types
open Domain.MeasureBuilder

let ``it generates events between two measures`` =
    testTheory
        "it generates events between two measures"
        [ { Id = "the current measure is the first measure"
            Data = None, emptyMeasure (MeasureNumber 1) |> cNatural |> commonTime
            ExpectedResult =
              [ { NaturalNote = NaturalNote.C
                  Accidental = Accidental.Natural }
                |> KeySignature
                |> MeasureEvent.DefineKeySignature

                { Numerator = 4
                  Denominator = Duration.QuarterNote }
                |> MeasureEvent.DefineTimeSignature ] } ]
    <| fun
           { Id = _
             Data = (previousMeasure, currentMeasure)
             ExpectedResult = expectedResult } ->
        let events = Measure.generateEvents previousMeasure currentMeasure

        events |> containsAll "The expected events were not found" expectedResult

[<Tests>]
let MeasureSpec =
    testList "MeasureSpec" [ ``it generates events between two measures`` ]
