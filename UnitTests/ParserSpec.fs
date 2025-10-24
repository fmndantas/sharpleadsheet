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
    case("c8").WithData(None, None, "c8").WithExpectedResult(Note.create4 NoteName.C Duration.Eighth)
    case("f16").WithData(None, None, "f16").WithExpectedResult(Note.create4 NoteName.F Duration.Sixteenth)

    case("f, there is a last note ~~> uses last note duration")
      .WithData(Some Duration.Whole, Pitch.create NoteName.C 3 |> Some, "f")
      .WithExpectedResult(Note.create4 NoteName.F Duration.Whole)

    case("f, there is not a last note ~~> uses current time signature denominator")
      .WithData(None, None, "f")
      .WithExpectedResult(Note.create4 NoteName.F Duration.Quarter)

    case("b1").WithData(None, None, "b1").WithExpectedResult(Note.create4 NoteName.B Duration.Whole)
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
        },
        openSample "sequence-of-notes-1.sls"
      )
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withClef Clef.G
          |> withCNaturalKeySignature
          |> withTimeSignature {
            Numerator = 2
            Denominator = Duration.Quarter
          }

        [
          measure
          |> withNotes [
            Note.create4 NoteName.C Duration.Eighth
            Note.create4 NoteName.D Duration.Eighth
            Note.create4 NoteName.E Duration.Eighth
            Note.create4 NoteName.D Duration.Eighth
          ]

          measure |> withNote (Note.create4 NoteName.C Duration.Half)
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
        },
        openSample "sequence-of-notes-2.sls"
      )
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withClef Clef.F
          |> withKeySignature (KeySignature NoteName.F)
          |> withTimeSignature {
            Numerator = 3
            Denominator = Duration.Quarter
          }

        [
          measure
          |> withNotes [
            Note.create4 NoteName.C Duration.Quarter
            Note.create4 NoteName.D Duration.Quarter
            Note.create4 NoteName.C Duration.Quarter
          ]

          measure
          |> withNotes [
            Note.create4 NoteName.F Duration.Half
            Note.create4 NoteName.G Duration.Quarter
          ]

          measure |> withRepeteadNote 6 (Note.create4 NoteName.E Duration.Eighth)

          measure
          |> withNotes [
            Note.create4 NoteName.C Duration.Half
            Note.create4 NoteName.D Duration.Quarter
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
        },
        openSample "sequence-of-notes-3.sls"
      )
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withClef Clef.G
          |> withCNaturalKeySignature
          |> withCommonTimeSignature

        [
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
          measure |> withNote (Note.create4 NoteName.G Duration.Whole)
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
          measure |> withNote (Note.create4 NoteName.G Duration.Whole)
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
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
        },
        openSample "sequence-of-notes-4.sls"
      )
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withClef Clef.G
          |> withCNaturalKeySignature
          |> withCommonTimeSignature

        [
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
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
        },
        openSample "sequence-of-notes-5.sls"
      )
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withCommonTimeSignature
          |> withCNaturalKeySignature
          |> withClef Clef.G

        [
          measure
          |> withNote (Note.create4 NoteName.C Duration.Half)
          |> withNote (Note.create5 NoteName.C Duration.Half)

          measure
          |> withNote (Note.create4 NoteName.B Duration.Quarter)
          |> withNote (Note.create4 NoteName.G Duration.Eighth)
          |> withNote (Note.create4 NoteName.A Duration.Eighth)
          |> withNote (Note.create4 NoteName.B Duration.Quarter)
          |> withNote (Note.create5 NoteName.C Duration.Quarter)

          measure
          |> withNote (Note.create4 NoteName.C Duration.Half)
          |> withNote (Note.create4 NoteName.A Duration.Half)

          measure |> withNote (Note.create4 NoteName.G Duration.Whole)

          measure
          |> withNote (Note.create2 NoteName.C Duration.Half)
          |> withNote (Note.create6 NoteName.C Duration.Half)
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
        },
        openSample "sequence-of-notes-6.sls"
      )
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withCommonTimeSignature
          |> withCNaturalKeySignature
          |> withClef Clef.G

        [
          measure
          |> withRest Duration.Quarter
          |> withRepeteadNote 2 (Note.create4 NoteName.C Duration.Quarter)
          |> withNote (Note.create4 NoteName.D Duration.Quarter)

          measure
          |> withNote (Note.create4 NoteName.E Duration.Quarter)
          |> withRest Duration.HalfDotted

          measure
          |> withRest Duration.Quarter
          |> withNote (Note.create4 NoteName.E Duration.Quarter)
          |> withNote (Note.create4 NoteName.F Duration.Quarter)
          |> withRest Duration.Eighth
          |> withNote (Note.create4 NoteName.E Duration.Eighth)

          measure
          |> withNote (Note.create4 NoteName.E Duration.Sixteenth)
          |> withRest Duration.EighthDotted
          |> withRest Duration.Quarter
          |> withNote (Note.create4 NoteName.D Duration.Half)

          measure
          |> withRest Duration.Quarter
          |> withNote (Note.createTied4 NoteName.D Duration.Quarter)
          |> withNote (Note.createTied4 NoteName.D Duration.Quarter)
          |> withNote (Note.create4 NoteName.D Duration.Eighth)
          |> withNote (Note.create4 NoteName.E Duration.Eighth)

          measure
          |> withNote (Note.createTied4 NoteName.F Duration.Quarter)
          |> withNote (Note.create4 NoteName.F Duration.Eighth)
          |> withNote (Note.create4 NoteName.E Duration.Eighth)
          |> withNote (Note.create4 NoteName.F Duration.Eighth)
          |> withNote (Note.createTied4 NoteName.E Duration.Eighth)
          |> withNote (Note.create4 NoteName.E Duration.Quarter)
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
        },
        openSample "notes-section-1.sls"
      )
      .WithExpectedResult
      {
        PartId = PartId 7
        Measures =
          let measure =
            aParsedMeasure () |> withCommonTimeSignature |> withCNaturalKeySignature

          [
            measure |> withNote (Note.create4 NoteName.G Duration.Whole)
            measure |> withNote (Note.create4 NoteName.C Duration.Whole)
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
        Parsed {
          PartDefinitionSections = [
            {
              Id = 1 |> PartId |> Some
              Name = Some "Piano"
              Clef = Some Clef.G
              TimeSignature =
                Some {
                  Numerator = 2
                  Denominator = Duration.Quarter
                }
              KeySignature = NoteName.C |> KeySignature |> Some
            }
          ]
          NotesSections = [
            {
              PartId = PartId 1
              Measures =
                let measure =
                  aParsedMeasure ()
                  |> withTimeSignature {
                    Numerator = 2
                    Denominator = Duration.Quarter
                  }
                  |> withCNaturalKeySignature

                [
                  measure
                  |> withNote (Note.create4 NoteName.C Duration.Eighth)
                  |> withNote (Note.create4 NoteName.D Duration.Eighth)
                  |> withNote (Note.create4 NoteName.E Duration.Eighth)
                  |> withNote (Note.create4 NoteName.D Duration.Eighth)

                  measure |> withNote (Note.create4 NoteName.C Duration.Half)

                  measure
                  |> withNote (Note.create4 NoteName.E Duration.Quarter)
                  |> withRest Duration.Quarter

                  measure
                  |> withNote (Note.create4 NoteName.F Duration.Eighth)
                  |> withNote (Note.create4 NoteName.G Duration.Sixteenth)
                  |> withRest Duration.Sixteenth
                  |> withNote (Note.create5 NoteName.AFlat Duration.EighthDotted)
                  |> withRest Duration.Sixteenth
                ]
            }
          ]
        },
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
        }
      )

    caseId(2)
      .WithData(openSample "example-2.sls")
      .WithExpectedResult(
        Parsed {
          PartDefinitionSections = [
            {
              Id = 2 |> PartId |> Some
              Name = Some "bass"
              Clef = Some Clef.F
              TimeSignature =
                Some {
                  Numerator = 1
                  Denominator = Duration.Eighth
                }
              KeySignature = NoteName.G |> KeySignature |> Some
            }
          ]
          NotesSections = [
            {
              PartId = PartId 2
              Measures =
                let measure =
                  aParsedMeasure ()
                  |> withTimeSignature {
                    Numerator = 1
                    Denominator = Duration.Eighth
                  }
                  |> withKeySignature (KeySignature NoteName.G)
                  |> withClef Clef.F

                [
                  measure |> withNote (Note.create4 NoteName.C Duration.Eighth)

                  measure
                  |> withNote (Note.create4 NoteName.G Duration.Sixteenth)
                  |> withNote (Note.create4 NoteName.F Duration.Sixteenth)

                  measure
                  |> withNote (Note.create4 NoteName.E Duration.Sixteenth)
                  |> withNote (Note.create4 NoteName.D Duration.Sixteenth)

                  measure |> withNote (Note.create4 NoteName.C Duration.Eighth)
                ]
            }
          ]
        },
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
