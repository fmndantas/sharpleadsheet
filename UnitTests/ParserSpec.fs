module UnitTests.ParserSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open FParsec

open Case

open Domain
open CommonTypes
open ParsedTypes
open ParsedMeasureBuilder
open ParserStateBuilder

[<Literal>]
let here = __SOURCE_DIRECTORY__

let private defaultSettings = {
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  Clef = Clef.G
}

let private openSample (file: string) : string =
  let dot = Directory.GetParent(here).FullName
  let file = Path.Join(dot, "Samples", file)
  File.ReadAllText file

let private runWithStateAndAssertOnSuccess parser initialState content assertFn =
  match runParserOnString parser initialState "runWithStateAndAssertOnSuccess" content with
  | Success(result, finalState, _) -> assertFn result finalState
  | Failure(errorMessage, _, _) -> failtest errorMessage

let private runAndAssertOnSuccess parser content assertFn =
  runWithStateAndAssertOnSuccess parser (aParserState ()) content assertFn

let private runAndAssertOnFailure parser content assertFn =
  match runParserOnString parser (aParserState ()) "runAndAssertOnFailure" content with
  | Success(_, _, _) -> failtest "Expected failure but got success"
  | Failure(errorMessage, _, _) -> assertFn errorMessage

let ``parses a part definition section`` =
  let sampleCase (id, sampleName) =
    (caseId id).WithData(openSample sampleName)

  testTheory3 "parses a part definition section" [
    sampleCase(1, "part-definition-1.sls").WithExpectedResult {
      Id = 1 |> PartId |> Some
      Name = Some "Piano"
      TimeSignature = {
        Numerator = 2
        Denominator = Duration.Quarter
      }
      KeySignature = KeySignature NoteName.F
      Clef = Clef.G
    }

    sampleCase(2, "part-definition-2.sls").WithExpectedResult {
      Id = 1 |> PartId |> Some
      Name = Some "guitar"
      TimeSignature = {
        Numerator = 1
        Denominator = Duration.Eighth
      }
      KeySignature = KeySignature NoteName.G
      Clef = Clef.G
    }

    sampleCase(3, "part-definition-3.sls").WithExpectedResult {
      Id = 2 |> PartId |> Some
      Name = Some "cifra"
      TimeSignature = {
        Numerator = 11
        Denominator = Duration.Eighth
      }
      KeySignature = KeySignature NoteName.BFlat
      Clef = Clef.F
    }
  ]
  <| fun content expectedResult ->
    runAndAssertOnSuccess (Parser.Functions.pPartDefinitionSection defaultSettings) content
    <| fun result _ -> result |> equal "part definition section is incorrect" expectedResult

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
    runAndAssertOnSuccess Parser.Functions.pNoteName data
    <| fun result _ -> result |> equal "note name is incorrect" expectedResult

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
    case("thirty-second note").WithData("32").WithExpectedResult Duration.ThirtySecond
  ]
  <| fun data expectedResult ->
    runAndAssertOnSuccess Parser.Functions.pDuration data
    <| fun result _ -> result |> equal "duration is incorrect" expectedResult

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
    let currentState =
      aParserState ()
      |> withOptionalLastPitch lastPitch
      |> withoptionalLastDuration lastDuration

    runWithStateAndAssertOnSuccess Parser.Functions.pNote currentState content
    <| fun result _ -> result |> equal "note is incorrect" expectedResult

let ``parses a rest`` =
  testTheory3 "parses a rest" [
    case("4").WithData(None, "r4").WithExpectedResult(Rest.create Duration.Quarter)
    case("8.").WithData(None, "r8.").WithExpectedResult(Rest.create Duration.EighthDotted)
    case("1").WithData(None, "r1").WithExpectedResult(Rest.create Duration.Whole)
    case("2.").WithData(Some Duration.HalfDotted, "r").WithExpectedResult(Rest.create Duration.HalfDotted)
    case("4.").WithData(None, "r").WithExpectedResult(Rest.create Duration.Sixteenth)
    case("1.").WithData(Some Duration.WholeDotted, "r16.").WithExpectedResult(Rest.create Duration.SixteenthDotted)
  ]
  <| fun (lastDuration, content) expectedResult ->
    let state =
      aParserState ()
      |> withCurrentTimeSignature {
        Numerator = 1
        Denominator = Duration.Sixteenth
      }
      |> withoptionalLastDuration lastDuration

    runWithStateAndAssertOnSuccess Parser.Functions.pRest state content
    <| fun result _ -> result |> equal "rest is incorrect" expectedResult

let ``parses a chord`` =
  testTheory3 "parses a chord" [
    caseId(1).WithData("c").WithExpectedResult(Chord.createWithRoot NoteName.C)
    caseId(2).WithData("b.maj7").WithExpectedResult(Chord.createWithKind NoteName.B "maj7")
    caseId(3).WithData("e/gs").WithExpectedResult(Chord.createWithBass NoteName.E NoteName.GSharp)
    caseId(4).WithData("f/bf").WithExpectedResult(Chord.createWithBass NoteName.F NoteName.BFlat)
    caseId(5)
      .WithData("fs.maj9(#11)/c")
      .WithExpectedResult(Chord.createWithBassAndKind NoteName.FSharp NoteName.C "maj9(#11)")
    caseId(6).WithData("f.add9/bf]").WithExpectedResult(Chord.createWithBassAndKind NoteName.F NoteName.BFlat "add9")
  ]
  <| fun content expectedResult ->
    runAndAssertOnSuccess Parser.Functions.pChord content
    <| fun result _ -> result |> equal "chord is incorrect" expectedResult

let ``parses notes section content`` =
  testTheory3 "parses notes section content" [
    caseId(1)
      .WithData(
        aParserState ()
        |> withCurrentTimeSignature {
          Numerator = 2
          Denominator = Duration.Quarter
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
        aParserState ()
        |> withCurrentTimeSignature {
          Numerator = 3
          Denominator = Duration.Quarter
        }
        |> withCurrentKeySignature (KeySignature NoteName.F)
        |> withCurrentClef Clef.F,
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
      .WithData(aParserState (), openSample "sequence-of-notes-3.sls")
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
      .WithData(aParserState (), openSample "sequence-of-notes-4.sls")
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
      .WithData(aParserState (), openSample "sequence-of-notes-5.sls")
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
      .WithData(aParserState (), openSample "sequence-of-notes-6.sls")
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withCommonTimeSignature
          |> withCNaturalKeySignature
          |> withClef Clef.G

        [
          measure
          |> withRest (Rest.create Duration.Quarter)
          |> withRepeteadNote 2 (Note.create4 NoteName.C Duration.Quarter)
          |> withNote (Note.create4 NoteName.D Duration.Quarter)

          measure
          |> withNote (Note.create4 NoteName.E Duration.Quarter)
          |> withRest (Rest.create Duration.HalfDotted)

          measure
          |> withRest (Rest.create Duration.Quarter)
          |> withNote (Note.create4 NoteName.E Duration.Quarter)
          |> withNote (Note.create4 NoteName.F Duration.Quarter)
          |> withRest (Rest.create Duration.Eighth)
          |> withNote (Note.create4 NoteName.E Duration.Eighth)

          measure
          |> withNote (Note.create4 NoteName.E Duration.Sixteenth)
          |> withRest (Rest.create Duration.EighthDotted)
          |> withRest (Rest.create Duration.Quarter)
          |> withNote (Note.create4 NoteName.D Duration.Half)

          measure
          |> withRest (Rest.create Duration.Quarter)
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

    case("7.chords-1")
      .WithData(aParserState (), openSample "chords-1.sls")
      .WithExpectedResult(
        let measure =
          aParsedMeasure ()
          |> withCommonTimeSignature
          |> withCNaturalKeySignature
          |> withClef Clef.G

        [
          measure
          |> withNote (
            Note.create4 NoteName.C Duration.Whole
            |> Note.withChord (Chord.createWithBassAndKind NoteName.C NoteName.G "maj9")
          )

          measure
          |> withRest (
            Rest.create Duration.Whole
            |> Rest.withChord (Chord.createWithBassAndKind NoteName.A NoteName.E "maj9(#11)")
          )
        ]
      )
  ]
  <| fun (currentState, content) expectedResult ->
    runWithStateAndAssertOnSuccess Parser.Functions.pNotesSectionContent currentState content
    <| fun result _ -> result |> equal "notes section content is incorrect" expectedResult

let ``parses notes section`` =
  testTheory3 "parses notes section" [
    caseId(1).WithData(aParserState (), openSample "notes-section-1.sls").WithExpectedResult {
      PartId = PartId 7
      Measures =
        let measure =
          aParsedMeasure () |> withCommonTimeSignature |> withCNaturalKeySignature

        [
          measure |> withNote (Note.create4 NoteName.G Duration.Whole)
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
        ]
    }

    caseId(2).WithData(aParserState (), openSample "notes-section-2.sls").WithExpectedResult {
      PartId = PartId 7
      Measures =
        let measure =
          aParsedMeasure () |> withCommonTimeSignature |> withCNaturalKeySignature

        [
          measure
          |> withNotes [
            Note.create4 NoteName.C Duration.Eighth
            Note.create4 NoteName.D Duration.Eighth
            Note.create4 NoteName.E Duration.Eighth
            Note.create4 NoteName.F Duration.Eighth
            Note.create4 NoteName.G Duration.Eighth
            Note.create4 NoteName.F Duration.Eighth
            Note.create4 NoteName.E Duration.Eighth
            Note.create4 NoteName.D Duration.Eighth
          ]
          measure |> withNote (Note.create4 NoteName.C Duration.Whole)
        ]
    }
  ]
  <| fun (currentState, content) expectedResult ->
    runWithStateAndAssertOnSuccess Parser.Functions.pNotesSection currentState content
    <| fun result _ -> result |> equal "notes section is incorrect" expectedResult

let ``parses music`` =
  testTheory3 "parses music" [
    caseId(1)
      .WithData(openSample "example-1.sls")
      .WithExpectedResult(
        {
          PartDefinitionSections = [
            {
              Id = 1 |> PartId |> Some
              Name = Some "Piano"
              Clef = Clef.G
              TimeSignature = {
                Numerator = 2
                Denominator = Duration.Quarter
              }
              KeySignature = KeySignature NoteName.C
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
                  |> withRest (Rest.create Duration.Quarter)

                  measure
                  |> withNote (Note.create4 NoteName.F Duration.Eighth)
                  |> withNote (Note.create4 NoteName.G Duration.Sixteenth)
                  |> withRest (Rest.create Duration.Sixteenth)
                  |> withNote (Note.create5 NoteName.AFlat Duration.EighthDotted)
                  |> withRest (Rest.create Duration.Sixteenth)
                ]
            }
          ]
        },
        aParserState ()
        |> withCurrentTimeSignature {
          Numerator = 2
          Denominator = Duration.Quarter
        }
        |> withCurrentOctave 5
        |> withLastPitch (Pitch.create NoteName.AFlat 5)
        |> withLastDuration Duration.Sixteenth
      )

    caseId(2)
      .WithData(openSample "example-2.sls")
      .WithExpectedResult(
        {
          PartDefinitionSections = [
            {
              Id = 2 |> PartId |> Some
              Name = Some "bass"
              Clef = Clef.F
              TimeSignature = {
                Numerator = 1
                Denominator = Duration.Eighth
              }
              KeySignature = KeySignature NoteName.G
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
        aParserState ()
        |> withCurrentTimeSignature {
          Numerator = 1
          Denominator = Duration.Eighth
        }
        |> withCurrentClef Clef.F
        |> withCurrentKeySignature (KeySignature NoteName.G)
        |> withCurrentOctave 4
        |> withLastPitch (Pitch.createMiddle NoteName.C)
        |> withLastDuration Duration.Eighth
      )

    caseId(3)
      .WithData(openSample "example-3.sls")
      .WithExpectedResult(
        {
          PartDefinitionSections = [
            {
              Id = 3 |> PartId |> Some
              Name = Some "Melodia"
              Clef = Clef.G
              TimeSignature = {
                Numerator = 4
                Denominator = Duration.Quarter
              }
              KeySignature = KeySignature NoteName.F
            }
          ]
          NotesSections = [
            {
              PartId = PartId 3
              Measures =
                let measure =
                  aParsedMeasure ()
                  |> withTimeSignature {
                    Numerator = 4
                    Denominator = Duration.Quarter
                  }
                  |> withKeySignature (KeySignature NoteName.F)
                  |> withClef Clef.G

                let quarterRest = Rest.create Duration.Quarter

                [
                  measure
                  |> withRest (quarterRest |> Rest.withChord (Chord.createWithKind NoteName.D "m9"))
                  |> withRepeatedRest 3 quarterRest

                  measure
                  |> withRest (quarterRest |> Rest.withChord (Chord.createWithKind NoteName.D "m9"))
                  |> withRepeatedRest 3 quarterRest

                  measure
                  |> withNotes [
                    Note.create4 NoteName.A Duration.Eighth
                    |> Note.withChord (Chord.createWithKind NoteName.D "m9")
                    Note.create5 NoteName.C Duration.Eighth
                    Note.create5 NoteName.C Duration.Eighth
                    Note.create5 NoteName.C Duration.Eighth
                    Note.create5 NoteName.C Duration.QuarterDotted
                    Note.create5 NoteName.D Duration.Eighth
                  ]

                  measure |> withNote (Note.create5 NoteName.C Duration.Whole)

                  measure
                  |> withNotes [
                    Note.create4 NoteName.A Duration.Eighth
                    |> Note.withChord (Chord.createWithKind NoteName.D "m9")
                    Note.create5 NoteName.C Duration.Eighth
                    Note.create5 NoteName.C Duration.Eighth
                    Note.create5 NoteName.C Duration.Eighth
                    Note.create5 NoteName.C Duration.QuarterDotted
                    Note.create5 NoteName.D Duration.Eighth
                  ]

                  measure |> withNote (Note.create5 NoteName.C Duration.Whole)

                  measure
                  |> withNotes [
                    Note.create5 NoteName.D Duration.Eighth
                    |> Note.withChord (Chord.createWithKind NoteName.G "m9")
                    Note.create5 NoteName.F Duration.Eighth
                    Note.create5 NoteName.F Duration.Eighth
                    Note.create5 NoteName.F Duration.Eighth
                    Note.create5 NoteName.F Duration.QuarterDotted
                    Note.create5 NoteName.G Duration.Eighth
                  ]

                  measure
                  |> withNote (Note.create5 NoteName.F Duration.HalfDotted)
                  |> withRest (Rest.create Duration.Eighth)
                  |> withNote (Note.create4 NoteName.BFlat Duration.Eighth)

                  measure
                  |> withNotes [
                    Note.create4 NoteName.A Duration.Eighth
                    |> Note.withChord (Chord.createWithKind NoteName.E "m9(11)")
                    Note.create4 NoteName.A Duration.Eighth
                    Note.create4 NoteName.A Duration.Eighth
                    Note.createTied4 NoteName.A Duration.Eighth
                    Note.create4 NoteName.A Duration.QuarterDotted
                    Note.create5 NoteName.D Duration.Eighth
                  ]

                  measure
                  |> withNotes [
                    Note.create4 NoteName.A Duration.Eighth
                    |> Note.withChord (Chord.createWithKind NoteName.EFlat "7(#11)")
                    Note.create4 NoteName.A Duration.Eighth
                    Note.create4 NoteName.A Duration.Quarter
                    Note.createTied4 NoteName.A Duration.QuarterDotted
                    Note.create4 NoteName.A Duration.Sixteenth
                    Note.create4 NoteName.A Duration.ThirtySecond
                    Note.create5 NoteName.C Duration.ThirtySecond
                  ]

                  measure
                  |> withNote (
                    Note.create4 NoteName.G Duration.Whole
                    |> Note.withChord (Chord.createWithKind NoteName.D "m9")
                  )
                ]
            }
          ]
        },
        aParserState ()
        |> withCurrentTimeSignature {
          Numerator = 4
          Denominator = Duration.Quarter
        }
        |> withCurrentKeySignature (KeySignature NoteName.F)
        |> withCurrentClef Clef.G
        |> withCurrentOctave 4
        |> withLastPitch (Pitch.createMiddle NoteName.G)
        |> withLastDuration Duration.Whole
      )
  ]
  <| fun content (expectedResult: ParsedMusic, expectedFinalState: ParserState) ->
    runAndAssertOnSuccess (Parser.Functions.pMusic defaultSettings) content
    <| fun result finalState ->
      result |> equal "music is incorrect" expectedResult
      finalState |> equal "final state is incorrect" expectedFinalState

let ``parses invalid music`` =
  testTheory3 "parses invalid music" [
    case("1.space between notes").WithData(openSample "invalid-music-1.sls").WithExpectedResult(8, 11)
    case("2.space between note and rest").WithData(openSample "invalid-music-2.sls").WithExpectedResult(8, 5)
    case("3.space between rest and note").WithData(openSample "invalid-music-3.sls").WithExpectedResult(8, 5)
    case("4.space between chord and note").WithData(openSample "invalid-music-4.sls").WithExpectedResult(8, 16)
    case("4.space between octave manipulation and note")
      .WithData(openSample "invalid-music-5.sls")
      .WithExpectedResult(10, 9)
  ]
  <| fun content (row, col) ->
    runAndAssertOnFailure (Parser.Functions.pMusic defaultSettings) content
    <| fun error ->
      error.ToLower()
      |> stringContains (sprintf "Expected position was not found") (sprintf "ln: %d col: %d" row col)

[<Tests>]
let ParserSpec =
  testList "parser" [
    ``parses a part definition section``
    ``parses a note name``
    ``parses a duration``
    ``parses a note``
    ``parses a rest``
    ``parses a chord``
    ``parses notes section content``
    ``parses notes section``
    ``parses music``
    ``parses invalid music``
  ]
