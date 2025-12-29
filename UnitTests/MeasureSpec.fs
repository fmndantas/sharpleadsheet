module UnitTests.MeasureSpec

open System

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open CommonTypes
open ParsedMeasureBuilder
open ValidatedMeasureBuilder
open Measure.Types

let ``generates measure events`` =
  let emptyMeasure =
    aParsedMeasure ()
    |> withCNaturalKeySignature
    |> withCommonTimeSignature
    |> withClef Clef.G

  let measureContext = {
    IsFirstMeasure = false
    IsTieStarted = false
    CurrentKeySignature = KeySignature NoteName.C
    CurrentTimeSignature = {
      Numerator = 4
      Denominator = Duration.Quarter
    }
    CurrentClef = Clef.G
    TotalNumberOfMeasures = Int32.MaxValue
    CurrentMeasureIndex = 0
  }

  let withNextMeasureIndex ctx = {
    ctx with
        CurrentMeasureIndex = ctx.CurrentMeasureIndex + 1
  }

  testTheory3 "generates measure events" [
    case("current measure is the first measure")
      .WithData(
        {
          measureContext with
              IsFirstMeasure = true
        },
        emptyMeasure
      )
      .WithExpectedResult(
        withNextMeasureIndex {
          measureContext with
              IsFirstMeasure = false
        },
        [
          NoteName.C |> KeySignature |> DefineKeySignatureEvent
          DefineTimeSignatureEvent {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          DefineClefEvent Clef.G
        ],
        []
      )

    yield! [
      case("current measure does not change key signature, time signature or clef")
        .WithData(measureContext, emptyMeasure)
        .WithExpectedResult(
          withNextMeasureIndex measureContext,
          [],
          [
            NoteName.C |> KeySignature |> DefineKeySignatureEvent
            {
              Numerator = 4
              Denominator = Duration.Quarter
            }
            |> DefineTimeSignatureEvent
            Clef.G |> DefineClefEvent
          ]
        )

      case("current measure changes key signature")
        .WithData(
          {
            measureContext with
                CurrentKeySignature = KeySignature NoteName.BFlat
          },
          emptyMeasure |> withKeySignature (KeySignature NoteName.FSharp)
        )
        .WithExpectedResult(
          withNextMeasureIndex {
            measureContext with
                CurrentKeySignature = KeySignature NoteName.FSharp
          },
          [ NoteName.FSharp |> KeySignature |> DefineKeySignatureEvent ],
          []
        )

      case("current measure changes time signature")
        .WithData(
          {
            measureContext with
                CurrentTimeSignature = {
                  Numerator = 4
                  Denominator = Duration.Quarter
                }
          },
          emptyMeasure
          |> withTimeSignature {
            Numerator = 6
            Denominator = Duration.Eighth
          }
        )
        .WithExpectedResult(
          withNextMeasureIndex {
            measureContext with
                CurrentTimeSignature = {
                  Numerator = 6
                  Denominator = Duration.Eighth
                }
          },
          [
            DefineTimeSignatureEvent {
              Numerator = 6
              Denominator = Duration.Eighth
            }
          ],
          []
        )

      case("current measure changes clef")
        .WithData(
          {
            measureContext with
                CurrentClef = Clef.G
          },
          emptyMeasure |> withClef Clef.F
        )
        .WithExpectedResult(
          withNextMeasureIndex {
            measureContext with
                CurrentClef = Clef.F
          },
          [ DefineClefEvent Clef.F ],
          []
        )
    ]

    case("notes without modifiers")
      .WithData(measureContext, emptyMeasure |> withRepeteadNote 4 (Note.create4 NoteName.C Duration.Quarter))
      .WithExpectedResult(
        withNextMeasureIndex measureContext,
        List.replicate
          4
          (Note.create4 NoteName.C Duration.Quarter
           |> NoteOrRest.Note
           |> Measure.CreateEvent.noteOrRestEvent),
        []
      )

    case("starting tie note")
      .WithData(measureContext, emptyMeasure |> withNote (Note.createTied4 NoteName.C Duration.Whole))
      .WithExpectedResult(
        withNextMeasureIndex {
          measureContext with
              IsTieStarted = true
        },
        [
          Note.createTied4 NoteName.C Duration.Whole
          |> NoteOrRest.Note
          |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StartTie ]
        ],
        []
      )

    case("ending tie note")
      .WithData(
        {
          measureContext with
              IsTieStarted = true
        },
        emptyMeasure |> withNote (Note.create4 NoteName.C Duration.Whole)
      )
      .WithExpectedResult(
        withNextMeasureIndex {
          measureContext with
              IsTieStarted = false
        },
        [
          Note.create4 NoteName.C Duration.Whole
          |> NoteOrRest.Note
          |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StopTie ]
        ],
        []
      )

    case("sequence of tied notes")
      .WithData(
        measureContext,
        emptyMeasure
        |> withRepeteadNote 3 (Note.createTied4 NoteName.C Duration.Quarter)
        |> withNote (Note.create4 NoteName.C Duration.Quarter)
      )
      .WithExpectedResult(
        withNextMeasureIndex measureContext,
        [
          Note.createTied4 NoteName.C Duration.Quarter
          |> NoteOrRest.Note
          |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StartTie ]

          Note.createTied4 NoteName.C Duration.Quarter
          |> NoteOrRest.Note
          |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StartTie; StopTie ]

          Note.create4 NoteName.C Duration.Quarter
          |> NoteOrRest.Note
          |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StopTie ]
        ],
        []
      )

    case("final barline")
      .WithData(
        {
          measureContext with
              TotalNumberOfMeasures = 132
              CurrentMeasureIndex = 131
        },
        emptyMeasure
      )
      .WithExpectedResult(
        {
          measureContext with
              TotalNumberOfMeasures = 132
              CurrentMeasureIndex = 132
        },
        [ FinalBarlineEvent ],
        []
      )
  ]
  <| fun (context, measure) (expectedContext, eventsShouldInclude, eventsShouldNotInclude) ->
    let events, context =
      measure |> toValidatedMeasure 1 |> Measure.generateEvents context

    for item in eventsShouldInclude do
      events
      |> contains (sprintf "expected measure event not found: \"%A\"" item) item

    for item in eventsShouldNotInclude do
      events
      |> List.contains item
      |> isFalse (sprintf "unexpected measure event found: \"%A\"" item)

    context |> equal "context is different from expected" expectedContext

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
    ``generates measure events``
    ``defines the number of divisions based on quarter note``
  ]
