module UnitTests.MeasureSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types
open Domain.MeasureBuilder

let ``generates events between two measures`` =
  let initialMeasure =
    aMeasure 1
    |> withCNaturalKeySignature
    |> withCommonTimeSignature
    |> withClef Clef.G

  testTheory2 "generates events between two measures" [
    {
      Id = "current measure is the first measure"
      Data = None, initialMeasure
      ExpectedResult =
        [
          NoteName.C |> KeySignature |> MeasureEvent.DefineKeySignature
          MeasureEvent.DefineTimeSignature {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          MeasureEvent.DefineClef Clef.G
        ],
        []
    }

    // Key signature, time signature and clef
    yield! [
      {
        Id = "current measure does not change key signature, time signature or clef"
        Data = Some initialMeasure, aMeasure 2 |> withCNaturalKeySignature |> withCommonTimeSignature
        ExpectedResult =
          [],
          [
            NoteName.C |> KeySignature |> MeasureEvent.DefineKeySignature
            MeasureEvent.DefineTimeSignature {
              Numerator = 4
              Denominator = Duration.Quarter
            }
            MeasureEvent.DefineClef Clef.G
          ]
      }

      {
        Id = "current measure changes key signature"
        Data =
          Some initialMeasure,
          aMeasure 2
          |> withKeySignature (KeySignature NoteName.D)
          |> withCommonTimeSignature
        ExpectedResult = [ NoteName.D |> KeySignature |> MeasureEvent.DefineKeySignature ], []
      }

      {
        Id = "current measure changes time signature"
        Data =
          Some initialMeasure,
          aMeasure 2
          |> withCNaturalKeySignature
          |> withTimeSignature {
            Numerator = 6
            Denominator = Duration.Eighth
          }
        ExpectedResult =
          [
            MeasureEvent.DefineTimeSignature {
              Numerator = 6
              Denominator = Duration.Eighth
            }
          ],
          []
      }

      {
        Id = "current measure changes clef"
        Data =
          Some initialMeasure,
          aMeasure 2
          |> withCNaturalKeySignature
          |> withCommonTimeSignature
          |> withClef Clef.F
        ExpectedResult = [ MeasureEvent.DefineClef Clef.F ], []
      }
    ]
  ]
  <| fun (previousMeasure, currentMeasure) (resultShouldInclude, resultShouldNotInclude) ->
    let events = Measure.generateEvents previousMeasure currentMeasure

    for item in resultShouldInclude do
      events |> contains $"Expected measure event not found: \"{item}\"" item

    for item in resultShouldNotInclude do
      List.contains item events
      |> isFalse $"Unexpected measure event found: \"{item}\""

let ``defines the number of divisions based on quarter note`` =
  let measureWithDurations durations =
    let notes = List.map (Note.create4 NoteName.C) durations

    aMeasure 1 |> withNotes notes

  testTheory2 "defines the number of divisions based on quarter note" [
    {
      Id = "Empty case"
      Data = measureWithDurations []
      ExpectedResult = 1
    }

    {
      Id = "w"
      Data = measureWithDurations [ Duration.Whole ]
      ExpectedResult = 1
    }

    {
      Id = "hh"
      Data = measureWithDurations [ Duration.Half; Duration.Half ]
      ExpectedResult = 1
    }

    {
      Id = "qqqq"
      Data = measureWithDurations [ Duration.Quarter; Duration.Quarter; Duration.Quarter; Duration.Quarter ]
      ExpectedResult = 1
    }

    {
      Id = "qqqee"
      Data =
        measureWithDurations [
          Duration.Quarter
          Duration.Quarter
          Duration.Quarter
          Duration.Eighth
          Duration.Eighth
        ]
      ExpectedResult = 2
    }

    {
      Id = "qeeqses"
      Data =
        measureWithDurations [
          Duration.Quarter
          Duration.Eighth
          Duration.Eighth
          Duration.Quarter
          Duration.Sixteenth
          Duration.Eighth
          Duration.Sixteenth
        ]
      ExpectedResult = 4
    }
  ]
  <| fun measure expectedResult ->
    let result = Measure.defineDivisions measure
    result |> equal "The calculated division is incorrect" expectedResult

[<Tests>]
let MeasureSpec =
  testList "MeasureSpec" [
    ``generates events between two measures``
    ``defines the number of divisions based on quarter note``
  ]
