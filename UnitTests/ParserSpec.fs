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

let private runWithStateAndAssert p initialState content assertFn =
    match runParserOnString p initialState "" content with
    | Success(result, finalState, _) -> assertFn result finalState
    | Failure(errorMessage, _, _) -> failtest errorMessage

let private parsingStateForTest =
    { InitialKeySignature = KeySignature NoteName.C
      InitialTimeSignature =
        { Numerator = 2
          Denominator = Duration.Quarter }
      InitialClef = Clef.G
      CurrentOctave = 4
      LastMeasureId = None
      LastNote = None }

let ``parses a part definition section`` =
    let sampleCase (id, sampleName) =
        (caseId id).WithData(openSample sampleName)

    testTheory3
        "parses a part definition section"
        [ sampleCase(1, "part-definition-1.sls")
              .WithExpectedResult(
                  { Id = PartId 1 |> Some
                    Name = Some "Piano"
                    Clef = Some Clef.G
                    TimeSignature =
                      Some
                          { Numerator = 2
                            Denominator = Duration.Quarter }
                    KeySignature = KeySignature NoteName.F |> Some }
              )

          sampleCase(2, "part-definition-2.sls")
              .WithExpectedResult(
                  { Id = PartId 1 |> Some
                    Name = Some "guitar"
                    Clef = Some Clef.G
                    TimeSignature =
                      Some
                          { Numerator = 1
                            Denominator = Duration.Eighth }
                    KeySignature = KeySignature NoteName.G |> Some }
              ) ]
    <| fun content expectedResult ->
        runWithStateAndAssert Parser.Functions.pPartDefinitionSection parsingStateForTest content
        <| fun result _ -> result |> equal "Part definition section is incorrect" expectedResult

// TODO: sharp and flat notes
let ``parses a note name`` =
    testTheory3
        "parses a note name"
        [ case("c").WithData("c").WithExpectedResult(NoteName.C)
          case("d").WithData("d").WithExpectedResult(NoteName.D)
          case("e").WithData("e").WithExpectedResult(NoteName.E)
          case("f").WithData("f").WithExpectedResult(NoteName.F)
          case("g").WithData("g").WithExpectedResult(NoteName.G)
          case("a").WithData("a").WithExpectedResult(NoteName.A)
          case("b").WithData("b").WithExpectedResult(NoteName.B) ]
    <| fun data expectedResult ->
        runWithStateAndAssert Parser.Functions.pNoteName parsingStateForTest data
        <| fun result _ -> result |> equal "Note name is incorrect" expectedResult

let ``parses a duration`` =
    testTheory3
        "parses a duration"
        [ case("whole note").WithData("1").WithExpectedResult(Duration.Whole)
          case("dotted whole note")
              .WithData("1.")
              .WithExpectedResult(Duration.WholeDotted)
          case("half note").WithData("2").WithExpectedResult(Duration.Half)
          case("dotted half note").WithData("2.").WithExpectedResult(Duration.HalfDotted)
          case("quarter note").WithData("4").WithExpectedResult(Duration.Quarter)
          case("dotted quarter note")
              .WithData("4.")
              .WithExpectedResult(Duration.QuarterDotted)
          case("eighth note").WithData("8").WithExpectedResult(Duration.Eighth)
          case("dotted eighth note")
              .WithData("8.")
              .WithExpectedResult(Duration.EighthDotted)
          case("sixteenth note").WithData("16").WithExpectedResult(Duration.Sixteenth)
          case("dotted sixteenth note")
              .WithData("16.")
              .WithExpectedResult(Duration.SixteenthDotted) ]
    <| fun data expectedResult ->
        runWithStateAndAssert Parser.Functions.pDuration parsingStateForTest data
        <| fun result _ -> result |> equal "Duration is incorrect" expectedResult

let ``parses a note`` =
    let aState lastNote =
        { InitialKeySignature = KeySignature NoteName.C
          InitialTimeSignature =
            { Numerator = 2
              Denominator = Duration.Quarter }
          InitialClef = Clef.G
          CurrentOctave = 4
          LastNote = lastNote
          LastMeasureId = None }

    testTheory3
        "parses a note"
        [ case("c8")
              .WithData(aState None, "c8")
              .WithExpectedResult(
                  { NoteName = NoteName.C
                    Octave = 4
                    Duration = Duration.Eighth }
              )

          case("f16")
              .WithData(aState None, "f16")
              .WithExpectedResult(
                  { NoteName = NoteName.F
                    Octave = 4
                    Duration = Duration.Sixteenth }
              )

          case("f, there is a last note ~~> uses last note duration")
              .WithData(
                  aState (
                      Some(
                          { NoteName = NoteName.C
                            Octave = 3
                            Duration = Duration.Whole }
                      )
                  ),
                  "f"
              )
              .WithExpectedResult(
                  { NoteName = NoteName.F
                    Octave = 4
                    Duration = Duration.Whole }
              )

          case("f, there is not a last note ~~> uses current time signature denominator")
              .WithData(aState None, "f")
              .WithExpectedResult(
                  { NoteName = NoteName.F
                    Octave = 4
                    Duration = Duration.Quarter }
              )

          case("b1")
              .WithData(aState None, "b1")
              .WithExpectedResult(
                  { NoteName = NoteName.B
                    Octave = 4
                    Duration = Duration.Whole }
              ) ]
    <| fun (currentState, content) expectedResult ->
        runWithStateAndAssert Parser.Functions.pNote currentState content
        <| fun result _ -> result |> equal "Note is incorrect" expectedResult

let ``parses notes section content`` =
    testTheory3
        "parses notes section content"
        [ caseId(1)
              .WithData(
                  { InitialTimeSignature =
                      { Numerator = 2
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.C
                    InitialClef = Clef.G
                    CurrentOctave = 4
                    LastNote = None
                    LastMeasureId = None },
                  openSample "sequence-of-notes-1.sls"
              )
              .WithExpectedResult(
                  let measure =
                      aMeasure
                      >> withClef Clef.G
                      >> withCNaturalKeySignature
                      >> withTimeSignature
                          { Numerator = 2
                            Denominator = Duration.Quarter }

                  [ measure 1
                    |> withNotes
                        [ { NoteName = NoteName.C
                            Octave = 4
                            Duration = Duration.Eighth }

                          { NoteName = NoteName.D
                            Octave = 4
                            Duration = Duration.Eighth }

                          { NoteName = NoteName.E
                            Octave = 4
                            Duration = Duration.Eighth }

                          { NoteName = NoteName.D
                            Octave = 4
                            Duration = Duration.Eighth } ]

                    measure 2
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Half } ]
              )

          caseId(2)
              .WithData(
                  { InitialTimeSignature =
                      { Numerator = 3
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.F
                    InitialClef = Clef.F
                    CurrentOctave = 4
                    LastNote = None
                    LastMeasureId = None },
                  openSample "sequence-of-notes-2.sls"
              )
              .WithExpectedResult(
                  let measure =
                      aMeasure
                      >> withClef Clef.F
                      >> withKeySignature (KeySignature NoteName.F)
                      >> withTimeSignature
                          { Numerator = 3
                            Denominator = Duration.Quarter }

                  [ measure 1
                    |> withNotes
                        [ { NoteName = NoteName.C
                            Octave = 4
                            Duration = Duration.Quarter }

                          { NoteName = NoteName.D
                            Octave = 4
                            Duration = Duration.Quarter }

                          { NoteName = NoteName.C
                            Octave = 4
                            Duration = Duration.Quarter } ]

                    measure 2
                    |> withNote
                        { NoteName = NoteName.F
                          Octave = 4
                          Duration = Duration.Half }
                    |> withNote
                        { NoteName = NoteName.G
                          Octave = 4
                          Duration = Duration.Quarter }

                    measure 3
                    |> withRepeteadNote
                        6
                        { NoteName = NoteName.E
                          Octave = 4
                          Duration = Duration.Eighth }

                    measure 4
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Half }
                    |> withNote

                        { NoteName = NoteName.D
                          Octave = 4
                          Duration = Duration.Quarter } ]
              )

          caseId(3)
              .WithData(
                  { InitialTimeSignature =
                      { Numerator = 4
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.C
                    InitialClef = Clef.G
                    CurrentOctave = 4
                    LastNote = None
                    LastMeasureId = None },
                  openSample "sequence-of-notes-3.sls"
              )
              .WithExpectedResult(
                  let measure =
                      aMeasure
                      >> withClef Clef.G
                      >> withCNaturalKeySignature
                      >> withCommonTimeSignature

                  [ measure 1
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Whole }

                    measure 2
                    |> withNote
                        { NoteName = NoteName.G
                          Octave = 4
                          Duration = Duration.Whole }

                    measure 3
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Whole }

                    measure 4
                    |> withNote
                        { NoteName = NoteName.G
                          Octave = 4
                          Duration = Duration.Whole }

                    measure 5
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Whole } ]
              )

          caseId(4)
              .WithData(
                  { InitialTimeSignature =
                      { Numerator = 4
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.C
                    InitialClef = Clef.G
                    CurrentOctave = 4
                    LastNote = None
                    LastMeasureId = None },
                  openSample "sequence-of-notes-4.sls"
              )
              .WithExpectedResult(
                  let measure =
                      aMeasure
                      >> withClef Clef.G
                      >> withCNaturalKeySignature
                      >> withCommonTimeSignature

                  [ measure 1
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Whole }

                    measure 2
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Whole } ]
              )

          caseId(5)
              .WithData(
                  { InitialTimeSignature =
                      { Numerator = 4
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.C
                    InitialClef = Clef.G
                    CurrentOctave = 4
                    LastNote = None
                    LastMeasureId = None },
                  openSample "sequence-of-notes-5.sls"
              )
              .WithExpectedResult(
                  let measure =
                      aMeasure
                      >> withCommonTimeSignature
                      >> withCNaturalKeySignature
                      >> withClef Clef.G

                  [ measure 1
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Half }
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 5
                          Duration = Duration.Half }

                    measure 2
                    |> withNote
                        { NoteName = NoteName.B
                          Octave = 4
                          Duration = Duration.Quarter }
                    |> withNote
                        { NoteName = NoteName.G
                          Octave = 4
                          Duration = Duration.Eighth }
                    |> withNote
                        { NoteName = NoteName.A
                          Octave = 4
                          Duration = Duration.Eighth }
                    |> withNote
                        { NoteName = NoteName.B
                          Octave = 4
                          Duration = Duration.Quarter }
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 5
                          Duration = Duration.Quarter }

                    measure 3
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 4
                          Duration = Duration.Half }
                    |> withNote
                        { NoteName = NoteName.A
                          Octave = 4
                          Duration = Duration.Half }

                    measure 4
                    |> withNote
                        { NoteName = NoteName.G
                          Octave = 4
                          Duration = Duration.Whole }

                    measure 5
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 2
                          Duration = Duration.Half }
                    |> withNote
                        { NoteName = NoteName.C
                          Octave = 6
                          Duration = Duration.Half } ]
              ) ]
    <| fun (currentState, content) expectedResult ->
        runWithStateAndAssert Parser.Functions.pNotesSectionContent currentState content
        <| fun result _ -> result |> equal "Notes section content is incorrect" expectedResult

let ``parses notes section`` =
    testTheory3
        "parses notes section"
        [ caseId(7)
              .WithData(
                  { InitialTimeSignature =
                      { Numerator = 4
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.C
                    InitialClef = Clef.G
                    CurrentOctave = 4
                    LastNote = None
                    LastMeasureId = None },
                  openSample "notes-section-1.sls"
              )
              .WithExpectedResult(
                  { PartId = PartId 7
                    Measures =
                      [ aMeasure 1
                        |> withCommonTimeSignature
                        |> withCNaturalKeySignature
                        |> withNote
                            { NoteName = NoteName.G
                              Octave = 4
                              Duration = Duration.Whole }

                        aMeasure 2
                        |> withCommonTimeSignature
                        |> withCNaturalKeySignature
                        |> withNote
                            { NoteName = NoteName.C
                              Octave = 4
                              Duration = Duration.Whole } ] }
              ) ]
    <| fun (currentState, content) expectedResult ->
        runWithStateAndAssert Parser.Functions.pNotesSection currentState content
        <| fun result _ -> result |> equal "Notes section is incorrect" expectedResult

let ``parses music`` =
    testTheory3
        "parses music"
        [

          caseId(1)
              .WithData(openSample "example-1.sls")
              .WithExpectedResult(
                  Music
                      [ { Name = "Piano"
                          Id = PartId 1
                          Measures =
                            [ aMeasure 1
                              |> withCNaturalKeySignature
                              |> withTimeSignature
                                  { Numerator = 2
                                    Denominator = Duration.Quarter }
                              |> withNote
                                  { NoteName = NoteName.C
                                    Octave = 4
                                    Duration = Duration.Eighth }
                              |> withNote
                                  { NoteName = NoteName.D
                                    Octave = 4
                                    Duration = Duration.Eighth }
                              |> withNote
                                  { NoteName = NoteName.E
                                    Octave = 4
                                    Duration = Duration.Eighth }
                              |> withNote
                                  { NoteName = NoteName.D
                                    Octave = 4
                                    Duration = Duration.Eighth }

                              aMeasure 2
                              |> withCNaturalKeySignature
                              |> withTimeSignature
                                  { Numerator = 2
                                    Denominator = Duration.Quarter }
                              |> withNote
                                  { NoteName = NoteName.C
                                    Octave = 4
                                    Duration = Duration.Half } ] } ],
                  { InitialTimeSignature =
                      { Numerator = 2
                        Denominator = Duration.Quarter }
                    InitialKeySignature = KeySignature NoteName.C
                    InitialClef = Clef.G
                    CurrentOctave = 4
                    LastNote =
                      Some
                          { NoteName = NoteName.C
                            Octave = 4
                            Duration = Duration.Half }
                    LastMeasureId = Some << MeasureId <| 2 }
              )

          caseId(2)
              .WithData(openSample "example-2.sls")
              .WithExpectedResult(
                  Music
                      [ { Name = "bass"
                          Id = PartId 1
                          Measures =
                            let measure =
                                aMeasure
                                >> withKeySignature (KeySignature NoteName.G)
                                >> withTimeSignature
                                    { Numerator = 1
                                      Denominator = Duration.Eighth }
                                >> withClef Clef.F

                            [ measure 1
                              |> withNote
                                  { NoteName = NoteName.C
                                    Octave = 4
                                    Duration = Duration.Eighth }

                              measure 2
                              |> withNote
                                  { NoteName = NoteName.G
                                    Octave = 4
                                    Duration = Duration.Sixteenth }
                              |> withNote
                                  { NoteName = NoteName.F
                                    Octave = 4
                                    Duration = Duration.Sixteenth }

                              measure 3
                              |> withNote
                                  { NoteName = NoteName.E
                                    Octave = 4
                                    Duration = Duration.Sixteenth }
                              |> withNote
                                  { NoteName = NoteName.D
                                    Octave = 4
                                    Duration = Duration.Sixteenth }

                              measure 4
                              |> withNote
                                  { NoteName = NoteName.C
                                    Octave = 4
                                    Duration = Duration.Eighth } ] } ],
                  { InitialTimeSignature =
                      { Numerator = 1
                        Denominator = Duration.Eighth }
                    InitialKeySignature = KeySignature NoteName.G
                    InitialClef = Clef.F
                    CurrentOctave = 4
                    LastNote =
                      Some
                          { NoteName = NoteName.C
                            Octave = 4
                            Duration = Duration.Eighth }
                    LastMeasureId = Some << MeasureId <| 4 }
              ) ]
    <| fun content (expectedResult: Music, expectedFinalState: ParsingState) ->
        runWithStateAndAssert Parser.Functions.pMusic parsingStateForTest content
        <| fun result finalState ->
            result |> equal "Music is incorrect" expectedResult
            finalState |> equal "Final state is incorrect" expectedFinalState

[<Tests>]
let ParserSpec =
    testList
        "ParserSpec"
        [ ``parses a part definition section``
          ``parses a note name``
          ``parses a duration``
          ``parses a note``
          ``parses notes section content``
          ``parses notes section``
          ``parses music`` ]
