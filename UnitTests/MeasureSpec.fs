module UnitTests.MeasureSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open Domain.CommonTypes
open Domain.ParsedMeasureBuilder
open Domain.ValidatedMeasureBuilder

let ``generates events between two measures`` =
  let initialMeasure =
    aParsedMeasure ()
    |> withCNaturalKeySignature
    |> withCommonTimeSignature
    |> withClef Clef.G

  testTheory3 "generates events between two measures" [
    case("current measure is the first measure")
      .WithData(None, initialMeasure)
      .WithExpectedResult(
        [
          NoteName.C |> KeySignature |> MeasureEvent.DefineKeySignature
          MeasureEvent.DefineTimeSignature {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          MeasureEvent.DefineClef Clef.G
        ],
        []
      )

    yield! [
      case("current measure does not change key signature, time signature or clef")
        .WithData(Some initialMeasure, aParsedMeasure () |> withCNaturalKeySignature |> withCommonTimeSignature)
        .WithExpectedResult(
          [],
          [
            NoteName.C |> KeySignature |> MeasureEvent.DefineKeySignature
            MeasureEvent.DefineTimeSignature {
              Numerator = 4
              Denominator = Duration.Quarter
            }
          ]
        )

      case("current measure changes key signature")
        .WithData(
          Some initialMeasure,
          aParsedMeasure ()
          |> withKeySignature (KeySignature NoteName.D)
          |> withCommonTimeSignature
        )
        .WithExpectedResult([ NoteName.D |> KeySignature |> MeasureEvent.DefineKeySignature ], [])

      case("current measure changes time signature")
        .WithData(
          Some initialMeasure,
          aParsedMeasure ()
          |> withCNaturalKeySignature
          |> withTimeSignature {
            Numerator = 6
            Denominator = Duration.Eighth
          }
        )
        .WithExpectedResult(
          [
            MeasureEvent.DefineTimeSignature {
              Numerator = 6
              Denominator = Duration.Eighth
            }
          ],
          []
        )

      case("current measure changes clef")
        .WithData(
          Some initialMeasure,
          aParsedMeasure ()
          |> withCNaturalKeySignature
          |> withCommonTimeSignature
          |> withClef Clef.F
        )
        .WithExpectedResult([ MeasureEvent.DefineClef Clef.F ], [])
    ]
  ]
  <| fun (previousMeasure, currentMeasure) (eventsShouldInclude, eventsShouldNotInclude) ->
    let events =
      Measure.generateEvents (Option.map (toValidatedMeasure 1) previousMeasure) (toValidatedMeasure 2 currentMeasure)

    for item in eventsShouldInclude do
      events
      |> contains (sprintf "expected measure event not found: \"%A\"" item) item

    for item in eventsShouldNotInclude do
      List.contains item events
      |> isFalse (sprintf "unexpected measure event found: \"%A\"" item)

let ``defines the number of divisions based on quarter note`` =
  let measureWithDurations durations =
    let notes = List.map (Note.create4 NoteName.C) durations

    aParsedMeasure () |> withNotes notes

  testTheory3 "defines the number of divisions based on quarter note" [
    case("empty case").WithData(measureWithDurations []).WithExpectedResult Duration.Quarter
    case("w").WithData(measureWithDurations [ Duration.Whole ]).WithExpectedResult Duration.Quarter
    case("hh").WithData(measureWithDurations [ Duration.Half; Duration.Half ]).WithExpectedResult Duration.Quarter
    case("qqqq").WithData(measureWithDurations (List.replicate 4 Duration.Quarter)).WithExpectedResult Duration.Quarter
    case("qqqee")
      .WithData(
        measureWithDurations [
          yield! List.replicate 3 Duration.Quarter
          yield! List.replicate 2 Duration.Eighth
        ]
      )
      .WithExpectedResult
      Duration.Eighth
    case("qeeqses")
      .WithData(
        measureWithDurations [
          Duration.Quarter
          Duration.Eighth
          Duration.Eighth
          Duration.Quarter
          Duration.Sixteenth
          Duration.Eighth
          Duration.Sixteenth
        ]
      )
      .WithExpectedResult
      Duration.Sixteenth
    case("w.").WithData(measureWithDurations [ Duration.WholeDotted ]).WithExpectedResult Duration.Quarter
    case("h.").WithData(measureWithDurations [ Duration.HalfDotted ]).WithExpectedResult Duration.Quarter
    case("q.").WithData(measureWithDurations [ Duration.QuarterDotted ]).WithExpectedResult Duration.Eighth
    case("e.").WithData(measureWithDurations [ Duration.EighthDotted ]).WithExpectedResult Duration.Sixteenth
    case("s.").WithData(measureWithDurations [ Duration.SixteenthDotted ]).WithExpectedResult Duration.ThirtySecond
    case("e.ssh.")
      .WithData(
        measureWithDurations [
          Duration.EighthDotted
          Duration.Sixteenth
          Duration.Sixteenth
          Duration.HalfDotted
        ]
      )
      .WithExpectedResult
      Duration.Sixteenth
    case("s.e.h.")
      .WithData(measureWithDurations [ Duration.SixteenthDotted; Duration.EighthDotted; Duration.HalfDotted ])
      .WithExpectedResult
      Duration.ThirtySecond
  ]
  <| fun measure expectedResult ->
    measure
    |> toValidatedMeasure 1
    |> Measure.defineDivisions
    |> equal "the calculated division is incorrect" expectedResult

[<Tests>]
let MeasureSpec =
  testList "measure" [
    ``generates events between two measures``
    ``defines the number of divisions based on quarter note``
  ]
