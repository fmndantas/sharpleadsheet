module UnitTests.ParserSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open FParsec

open Case

open Domain
open Domain.Types
open Domain.MeasureBuilder

open Domain.Parser.Types

[<Literal>]
let here = __SOURCE_DIRECTORY__

let private openSample (file: string) =
  let dot = Directory.GetParent(here).FullName
  File.ReadAllText(Path.Join(dot, "Samples", file))

let private runWithStateAndAssert parser initialState content assertFn =
  match runParserOnString parser initialState "" content with
  | Success(result, finalState, _) -> assertFn result finalState
  | Failure(errorMessage, _, _) -> failtest errorMessage

let private runAndAssert parser content assertFn =
  let initialState = {
    InitialKeySignature = KeySignature NoteName.C
    InitialTimeSignature = {
      Numerator = 2
      Denominator = Duration.Quarter
    }
    InitialClef = Clef.G
    CurrentOctave = 4
    LastMeasureId = None
    LastDuration = None
    LastPitch = None
  }

  runWithStateAndAssert parser initialState content assertFn

let ``parses a part definition section`` =
  let sampleCase (id, sampleName) =
    (caseId id).WithData(openSample sampleName)

  testTheory3 "parses a part definition section" [
    sampleCase(1, "part-definition-1.sls").WithExpectedResult {
      Id = PartId 1 |> Some
      Name = Some "Piano"
      Clef = Some Clef.G
      TimeSignature =
        Some {
          Numerator = 2
          Denominator = Duration.Quarter
        }
      KeySignature = KeySignature NoteName.F |> Some
    }

    sampleCase(2, "part-definition-2.sls").WithExpectedResult {
      Id = PartId 1 |> Some
      Name = Some "guitar"
      Clef = Some Clef.G
      TimeSignature =
        Some {
          Numerator = 1
          Denominator = Duration.Eighth
        }
      KeySignature = KeySignature NoteName.G |> Some
    }
  ]
  <| fun content expectedResult ->
    runAndAssert Parser.Functions.pPartDefinitionSection content
    <| fun result _ -> result |> equal "Part definition section is incorrect" expectedResult

let ``parses a note name`` =
  testTheory3 "parses a note name" [
    case("C").WithData("c").WithExpectedResult NoteName.C
    case("C#").WithData("cs").WithExpectedResult NoteName.CSharp
    case("Db").WithData("df").WithExpectedResult NoteName.DFlat
    case("D").WithData("d").WithExpectedResult NoteName.D
    case("D#").WithData("ds").WithExpectedResult NoteName.DSharp
    case("Eb").WithData("ef").WithExpectedResult NoteName.EFlat
    case("E").WithData("e").WithExpectedResult NoteName.E
    case("F").WithData("f").WithExpectedResult NoteName.F
    case("F#").WithData("fs").WithExpectedResult NoteName.FSharp
    case("Gb").WithData("gf").WithExpectedResult NoteName.GFlat
    case("G").WithData("g").WithExpectedResult NoteName.G
    case("G#").WithData("gs").WithExpectedResult NoteName.GSharp
    case("Ab").WithData("af").WithExpectedResult NoteName.AFlat
    case("A").WithData("a").WithExpectedResult NoteName.A
    case("A#").WithData("as").WithExpectedResult NoteName.ASharp
    case("Bb").WithData("bf").WithExpectedResult NoteName.BFlat
    case("B").WithData("b").WithExpectedResult NoteName.B
  ]
  <| fun data expectedResult ->
    runAndAssert Parser.Functions.pNoteName data
    <| fun result _ -> result |> equal "Note name is incorrect" expectedResult

let ``parses a duration`` =
  testTheory3 "parses a duration" [
    case("whole note").WithData("1").WithExpectedResult Duration.Whole
    case("dotted whole note").WithData("1.").WithExpectedResult Duration.WholeDotted
    case("half note").WithData("2").WithExpectedResult Duration.Half
    case("dotted half note").WithData("2.").WithExpectedResult Duration.HalfDotted
    case("quarter note").WithData("4").WithExpectedResult Duration.Quarter
    case("dotted quarter note").WithData("4.").WithExpectedResult Duration.QuarterDotted
    case("eighth note").WithData("8").WithExpectedResult Duration.Eighth
    case("dotted eighth note").WithData("8.").WithExpectedResult Duration.EighthDotted
    case("sixteenth note").WithData("16").WithExpectedResult Duration.Sixteenth
    case("dotted sixteenth note").WithData("16.").WithExpectedResult Duration.SixteenthDotted
  ]
  <| fun data expectedResult ->
    runAndAssert Parser.Functions.pDuration data
    <| fun result _ -> result |> equal "Duration is incorrect" expectedResult

let ``parses a note`` =
  testTheory3 "parses a note" [
    case("c8").WithData(None, None, "c8").WithExpectedResult(Note.createMiddle NoteName.C Duration.Eighth)
    case("f16").WithData(None, None, "f16").WithExpectedResult(Note.createMiddle NoteName.F Duration.Sixteenth)

    case("f, there is a last note ~~> uses last note duration")
      .WithData(Some Duration.Whole, Pitch.create NoteName.C 3 |> Some, "f")
      .WithExpectedResult(Note.createMiddle NoteName.F Duration.Whole)

    case("f, there is not a last note ~~> uses current time signature denominator")
      .WithData(None, None, "f")
      .WithExpectedResult(Note.createMiddle NoteName.F Duration.Quarter)

    case("b1").WithData(None, None, "b1").WithExpectedResult(Note.createMiddle NoteName.B Duration.Whole)
  ]
  <| fun (lastDuration, lastPitch, content) expectedResult ->
    let currentState = {
      InitialKeySignature = KeySignature NoteName.C
      InitialTimeSignature = {
        Numerator = 2
        Denominator = Duration.Quarter
      }
      InitialClef = Clef.G
      CurrentOctave = 4
      LastPitch = lastPitch
      LastDuration = lastDuration
      LastMeasureId = None
    }

    runWithStateAndAssert Parser.Functions.pNote currentState content
    <| fun result _ -> result |> equal "Note is incorrect" expectedResult

let ``parses a rest`` =
  testTheory3 "parses a rest" [
    case("4").WithData(None, "r4").WithExpectedResult(Rest Duration.Quarter)
    case("8.").WithData(None, "r8.").WithExpectedResult(Rest Duration.EighthDotted)
    case("1").WithData(None, "r1").WithExpectedResult(Rest Duration.Whole)
    case("2.").WithData(Some Duration.HalfDotted, "r").WithExpectedResult(Rest Duration.HalfDotted)
    case("4.").WithData(None, "r").WithExpectedResult(Rest Duration.Sixteenth)
    case("1.").WithData(Some Duration.WholeDotted, "r16.").WithExpectedResult(Rest Duration.SixteenthDotted)
  ]
  <| fun (lastDuration, content) expectedResult ->
    let state = {
      InitialTimeSignature = {
        Numerator = 1
        Denominator = Duration.Sixteenth
      }
      InitialKeySignature = NoteName.C |> KeySignature
      InitialClef = Clef.G
      CurrentOctave = 4
      LastPitch = None
      LastDuration = lastDuration
      LastMeasureId = None
    }

    runWithStateAndAssert Parser.Functions.pRest state content
    <| fun result _ -> result |> equal "Rest is incorrect" expectedResult

let ``parses notes section content`` =
  testTheory3 "parses notes section content" [
    caseId(1)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 2
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "sequence-of-notes-1.sls"
      )
      .WithExpectedResult(
        let measure =
          aMeasure
          >> withClef Clef.G
          >> withCNaturalKeySignature
          >> withTimeSignature {
            Numerator = 2
            Denominator = Duration.Quarter
          }

        [
          measure 1
          |> withNotes [
            Note.createMiddle NoteName.C Duration.Eighth
            Note.createMiddle NoteName.D Duration.Eighth
            Note.createMiddle NoteName.E Duration.Eighth
            Note.createMiddle NoteName.D Duration.Eighth
          ]

          measure 2 |> withNote (Note.createMiddle NoteName.C Duration.Half)
        ]
      )

    caseId(2)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 3
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.F
          InitialClef = Clef.F
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "sequence-of-notes-2.sls"
      )
      .WithExpectedResult(
        let measure =
          aMeasure
          >> withClef Clef.F
          >> withKeySignature (KeySignature NoteName.F)
          >> withTimeSignature {
            Numerator = 3
            Denominator = Duration.Quarter
          }

        [
          measure 1
          |> withNotes [
            Note.createMiddle NoteName.C Duration.Quarter
            Note.createMiddle NoteName.D Duration.Quarter
            Note.createMiddle NoteName.C Duration.Quarter
          ]

          measure 2
          |> withNotes [
            Note.createMiddle NoteName.F Duration.Half
            Note.createMiddle NoteName.G Duration.Quarter
          ]

          measure 3 |> withRepeteadNote 6 (Note.createMiddle NoteName.E Duration.Eighth)

          measure 4
          |> withNotes [
            Note.createMiddle NoteName.C Duration.Half
            Note.createMiddle NoteName.D Duration.Quarter
          ]
        ]
      )

    caseId(3)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "sequence-of-notes-3.sls"
      )
      .WithExpectedResult(
        let measure =
          aMeasure
          >> withClef Clef.G
          >> withCNaturalKeySignature
          >> withCommonTimeSignature

        [
          measure 1 |> withNote (Note.createMiddle NoteName.C Duration.Whole)
          measure 2 |> withNote (Note.createMiddle NoteName.G Duration.Whole)
          measure 3 |> withNote (Note.createMiddle NoteName.C Duration.Whole)
          measure 4 |> withNote (Note.createMiddle NoteName.G Duration.Whole)
          measure 5 |> withNote (Note.createMiddle NoteName.C Duration.Whole)
        ]
      )

    caseId(4)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "sequence-of-notes-4.sls"
      )
      .WithExpectedResult(
        let measure =
          aMeasure
          >> withClef Clef.G
          >> withCNaturalKeySignature
          >> withCommonTimeSignature

        [
          measure 1 |> withNote (Note.createMiddle NoteName.C Duration.Whole)
          measure 2 |> withNote (Note.createMiddle NoteName.C Duration.Whole)
        ]
      )

    caseId(5)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "sequence-of-notes-5.sls"
      )
      .WithExpectedResult(
        let measure =
          aMeasure
          >> withCommonTimeSignature
          >> withCNaturalKeySignature
          >> withClef Clef.G

        [
          measure 1
          |> withNote (Note.createMiddle NoteName.C Duration.Half)
          |> withNote (Note.create NoteName.C 5 Duration.Half)

          measure 2
          |> withNote (Note.createMiddle NoteName.B Duration.Quarter)
          |> withNote (Note.createMiddle NoteName.G Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.A Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.B Duration.Quarter)
          |> withNote (Note.create NoteName.C 5 Duration.Quarter)

          measure 3
          |> withNote (Note.createMiddle NoteName.C Duration.Half)
          |> withNote (Note.createMiddle NoteName.A Duration.Half)

          measure 4 |> withNote (Note.createMiddle NoteName.G Duration.Whole)

          measure 5
          |> withNote (Note.create NoteName.C 2 Duration.Half)
          |> withNote (Note.create NoteName.C 6 Duration.Half)
        ]
      )

    caseId(6)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "sequence-of-notes-6.sls"
      )
      .WithExpectedResult(
        let measure =
          aMeasure
          >> withCommonTimeSignature
          >> withCNaturalKeySignature
          >> withClef Clef.G

        [
          measure 1
          |> withRest Duration.Quarter
          |> withRepeteadNote 2 (Note.createMiddle NoteName.C Duration.Quarter)
          |> withNote (Note.createMiddle NoteName.D Duration.Quarter)

          measure 2
          |> withNote (Note.createMiddle NoteName.E Duration.Quarter)
          |> withRest Duration.HalfDotted

          measure 3
          |> withRest Duration.Quarter
          |> withNote (Note.createMiddle NoteName.E Duration.Quarter)
          |> withNote (Note.createMiddle NoteName.F Duration.Quarter)
          |> withRest Duration.Eighth
          |> withNote (Note.createMiddle NoteName.E Duration.Eighth)

          measure 4
          |> withNote (Note.createMiddle NoteName.E Duration.Sixteenth)
          |> withRest Duration.EighthDotted
          |> withRest Duration.Quarter
          |> withNote (Note.createMiddle NoteName.D Duration.Half)

          measure 5
          |> withRest Duration.Quarter
          |> withNote (Note.createMiddle NoteName.D Duration.Quarter)
          |> withNote (Note.createMiddle NoteName.D Duration.Quarter)
          |> withNote (Note.createMiddle NoteName.D Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.E Duration.Eighth)

          measure 6
          |> withNote (Note.createMiddle NoteName.F Duration.Quarter)
          |> withNote (Note.createMiddle NoteName.F Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.E Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.F Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.E Duration.Eighth)
          |> withNote (Note.createMiddle NoteName.E Duration.Quarter)
        ]
      )
  ]
  <| fun (currentState, content) expectedResult ->
    runWithStateAndAssert Parser.Functions.pNotesSectionContent currentState content
    <| fun result _ -> result |> equal "Notes section content is incorrect" expectedResult

let ``parses notes section`` =
  testTheory3 "parses notes section" [
    caseId(7)
      .WithData(
        {
          InitialTimeSignature = {
            Numerator = 4
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        },
        openSample "notes-section-1.sls"
      )
      .WithExpectedResult
      {
        PartId = PartId 7
        Measures = [
          aMeasure 1
          |> withCommonTimeSignature
          |> withCNaturalKeySignature
          |> withNote (Note.createMiddle NoteName.G Duration.Whole)

          aMeasure 2
          |> withCommonTimeSignature
          |> withCNaturalKeySignature
          |> withNote (Note.createMiddle NoteName.C Duration.Whole)
        ]
      }
  ]
  <| fun (currentState, content) expectedResult ->
    runWithStateAndAssert Parser.Functions.pNotesSection currentState content
    <| fun result _ -> result |> equal "Notes section is incorrect" expectedResult

let ``parses music`` =
  testTheory3 "parses music" [
    caseId(1)
      .WithData(openSample "example-1.sls")
      .WithExpectedResult(
        Music [
          {
            Name = "Piano"
            Id = PartId 1
            Measures = [
              aMeasure 1
              |> withCNaturalKeySignature
              |> withTimeSignature {
                Numerator = 2
                Denominator = Duration.Quarter
              }
              |> withNote (Note.createMiddle NoteName.C Duration.Eighth)
              |> withNote (Note.createMiddle NoteName.D Duration.Eighth)
              |> withNote (Note.createMiddle NoteName.E Duration.Eighth)
              |> withNote (Note.createMiddle NoteName.D Duration.Eighth)

              aMeasure 2
              |> withCNaturalKeySignature
              |> withTimeSignature {
                Numerator = 2
                Denominator = Duration.Quarter
              }
              |> withNote (Note.createMiddle NoteName.C Duration.Half)

              aMeasure 3
              |> withCNaturalKeySignature
              |> withTimeSignature {
                Numerator = 2
                Denominator = Duration.Quarter
              }
              |> withNote (Note.createMiddle NoteName.E Duration.Quarter)
              |> withRest Duration.Quarter

              aMeasure 4
              |> withCNaturalKeySignature
              |> withTimeSignature {
                Numerator = 2
                Denominator = Duration.Quarter
              }
              |> withNote (Note.createMiddle NoteName.F Duration.Eighth)
              |> withNote (Note.createMiddle NoteName.G Duration.Sixteenth)
              |> withRest Duration.Sixteenth
              |> withNote (Note.create NoteName.AFlat 5 Duration.EighthDotted)
              |> withRest Duration.Sixteenth
            ]
          }
        ],
        {
          InitialTimeSignature = {
            Numerator = 2
            Denominator = Duration.Quarter
          }
          InitialKeySignature = KeySignature NoteName.C
          InitialClef = Clef.G
          CurrentOctave = 5
          LastPitch = Pitch.create NoteName.AFlat 5 |> Some
          LastDuration = Some Duration.Sixteenth
          LastMeasureId = 4 |> MeasureId |> Some
        }
      )

    caseId(2)
      .WithData(openSample "example-2.sls")
      .WithExpectedResult(
        Music [
          {
            Name = "bass"
            Id = PartId 1
            Measures =
              let measure =
                aMeasure
                >> withKeySignature (KeySignature NoteName.G)
                >> withTimeSignature {
                  Numerator = 1
                  Denominator = Duration.Eighth
                }
                >> withClef Clef.F

              [
                measure 1 |> withNote (Note.createMiddle NoteName.C Duration.Eighth)

                measure 2
                |> withNote (Note.createMiddle NoteName.G Duration.Sixteenth)
                |> withNote (Note.createMiddle NoteName.F Duration.Sixteenth)

                measure 3
                |> withNote (Note.createMiddle NoteName.E Duration.Sixteenth)
                |> withNote (Note.createMiddle NoteName.D Duration.Sixteenth)

                measure 4 |> withNote (Note.createMiddle NoteName.C Duration.Eighth)
              ]
          }
        ],
        {
          InitialTimeSignature = {
            Numerator = 1
            Denominator = Duration.Eighth
          }
          InitialKeySignature = KeySignature NoteName.G
          InitialClef = Clef.F
          CurrentOctave = 4
          LastPitch = Pitch.createMiddle NoteName.C |> Some
          LastDuration = Some Duration.Eighth
          LastMeasureId = 4 |> MeasureId |> Some
        }
      )
  ]
  <| fun content (expectedResult: Music, expectedFinalState: ParserState) ->
    runAndAssert Parser.Functions.pMusic content
    <| fun result finalState ->
      result |> equal "Music is incorrect" expectedResult
      finalState |> equal "Final state is incorrect" expectedFinalState

[<Tests>]
let ParserSpec =
  testList "ParserSpec" [
    ``parses a part definition section``
    ``parses a note name``
    ``parses a duration``
    ``parses a note``
    ``parses a rest``
    ``parses notes section content``
    ``parses notes section``
    ``parses music``
  ]
