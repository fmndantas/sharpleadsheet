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
  <| fun (previousMeasure, currentMeasure) (resultShouldInclude, resultShouldNotInclude) ->
    let events =
      Measure.generateEvents (Option.map (toValidatedMeasure 1) previousMeasure) (toValidatedMeasure 2 currentMeasure)

    for item in resultShouldInclude do
      events |> contains $"expected measure event not found: \"{item}\"" item

    for item in resultShouldNotInclude do
      List.contains item events
      |> isFalse $"unexpected measure event found: \"{item}\""

let ``defines the number of divisions based on quarter note`` =
  let measureWithDurations durations =
    let notes = List.map (Note.create4 NoteName.C) durations

    aParsedMeasure () |> withNotes notes

  testTheory3 "defines the number of divisions based on quarter note" [
    case("empty case").WithData(measureWithDurations []).WithExpectedResult 1
    case("w").WithData(measureWithDurations [ Duration.Whole ]).WithExpectedResult 1
    case("hh").WithData(measureWithDurations [ Duration.Half; Duration.Half ]).WithExpectedResult 1
    case("qqqq").WithData(measureWithDurations (List.replicate 4 Duration.Quarter)).WithExpectedResult 1
    case("qqqee")
      .WithData(
        measureWithDurations [
          yield! List.replicate 3 Duration.Quarter
          yield! List.replicate 2 Duration.Eighth
        ]
      )
      .WithExpectedResult
      2
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
      4
  ]
  <| fun measure expectedResult ->
    let result = Measure.defineDivisions (toValidatedMeasure 1 measure)
    result |> equal "the calculated division is incorrect" expectedResult

[<Tests>]
let MeasureSpec =
  testList "measure" [
    ``generates events between two measures``
    ``defines the number of divisions based on quarter note``
  ]
