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
            Data =
              Clef.G,
              None,
              emptyMeasure >> withCNaturalKeySignature >> withCommonTimeSignature
              <| MeasureNumber 1
            ExpectedResult =
              [ KeySignature >> MeasureEvent.DefineKeySignature <| NoteName.C

                MeasureEvent.DefineTimeSignature
                    { Numerator = 4
                      Denominator = Duration.QuarterNote }

                MeasureEvent.DefineClef Clef.G ] } ]
    <| fun (initialClef, previousMeasure, currentMeasure) (expectedResult) ->
        let events = Measure.generateEvents initialClef previousMeasure currentMeasure

        events |> containsAll "The expected events were not found" expectedResult

let ``defines the number of divisions based on quarter note`` =
    let measureWithDurations durations =
        let notes =
            List.map
                (fun d ->
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = d })
                durations

        emptyMeasure (MeasureNumber 1) |> withNotes notes

    tt
        "defines the number of divisions based on quarter note"
        [

          { Id = "Empty case"
            Data = measureWithDurations []
            ExpectedResult = 1 }

          { Id = "w"
            Data = measureWithDurations [ Duration.WholeNote ]
            ExpectedResult = 1 }

          { Id = "hh"
            Data = measureWithDurations [ Duration.HalfNote; Duration.HalfNote ]
            ExpectedResult = 1 }

          { Id = "qqqq"
            Data =
              measureWithDurations
                  [ Duration.QuarterNote
                    Duration.QuarterNote
                    Duration.QuarterNote
                    Duration.QuarterNote ]
            ExpectedResult = 1 }

          { Id = "qqqee"
            Data =
              measureWithDurations
                  [ Duration.QuarterNote
                    Duration.QuarterNote
                    Duration.QuarterNote
                    Duration.EightNote
                    Duration.EightNote ]
            ExpectedResult = 2 }

          { Id = "qeeqses"
            Data =
              measureWithDurations
                  [ Duration.QuarterNote
                    Duration.EightNote
                    Duration.EightNote
                    Duration.QuarterNote
                    Duration.SixteenthNote
                    Duration.EightNote
                    Duration.SixteenthNote ]
            ExpectedResult = 4 }

          ]
    <| fun measure expectedResult ->
        let result = Measure.defineDivisions measure
        result |> equal "The calculated division is incorrect" expectedResult

[<Tests>]
let MeasureSpec =
    testList
        "MeasureSpec"
        [ ``it generates events between two measures``
          ``defines the number of divisions based on quarter note`` ]
