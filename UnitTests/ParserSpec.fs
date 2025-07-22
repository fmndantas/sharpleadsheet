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

let private runAndAssert p content assertFn =
    match run p content with
    | Success(result, _, _) -> assertFn result
    | Failure(errorMessage, _, _) -> failtest errorMessage

let private runWithStateAndAssert p initialState content assertFn =
    match runParserOnString p initialState "" content with
    | Success(result, finalState, _) -> assertFn result finalState
    | Failure(errorMessage, _, _) -> failtest errorMessage

let private parsingStateForTest =
    { InitialKeySignature = KeySignature NoteName.C
      InitialTimeSignature =
        { Numerator = 2
          Denominator = Duration.QuarterNote }
      InitialClef = Clef.G
      LastMeasure = None
      LastNote = None }

let ``parses music`` =
    tt
        "parses music"
        [

          { Id = "one measure"
            Data = openSample "example-1.sls"
            ExpectedResult =
              Music
                  [ { Name = "Piano"
                      Id = PartId 1
                      Measures =
                        [ aMeasure 1
                          |> withCNaturalKeySignature
                          |> withTimeSignature
                              { Numerator = 2
                                Denominator = Duration.QuarterNote }
                          |> withNote
                              { NoteName = NoteName.C
                                Octave = 4
                                Duration = Duration.EighthNote }
                          |> withNote
                              { NoteName = NoteName.D
                                Octave = 4
                                Duration = Duration.EighthNote }
                          |> withNote
                              { NoteName = NoteName.E
                                Octave = 4
                                Duration = Duration.EighthNote }
                          |> withNote
                              { NoteName = NoteName.D
                                Octave = 4
                                Duration = Duration.EighthNote }

                          aMeasure 2
                          |> withCNaturalKeySignature
                          |> withTimeSignature
                              { Numerator = 2
                                Denominator = Duration.QuarterNote }
                          |> withNote
                              { NoteName = NoteName.C
                                Octave = 4
                                Duration = Duration.HalfNote } ] } ] }

          ]
    <| fun (fileContent) (expectedResult: Music) ->
        runWithStateAndAssert Parser.Functions.pMusic parsingStateForTest fileContent
        <| fun result _ -> result |> equal "Parsed music is incorrect" expectedResult

let ``parses a part definition`` =
    testCase "parses a part definition"
    <| fun () ->
        let part = openSample "part-definition-1.sls"

        let expectedResult =
            { Id = PartId 1 |> Some
              Name = Some "Piano"
              Clef = Some Clef.G
              TimeSignature =
                Some
                    { Numerator = 2
                      Denominator = Duration.QuarterNote }
              KeySignature = KeySignature NoteName.F |> Some }

        runWithStateAndAssert Parser.Functions.pPartDefinition parsingStateForTest part
        <| fun result _ -> result |> equal "Part definition is incorrect" expectedResult

// TODO: sharp and flat notes
let ``parses a note name`` =
    tt
        "parses a note name"
        [

          { Id = "c"
            Data = "c"
            ExpectedResult = NoteName.C }

          { Id = "d"
            Data = "d"
            ExpectedResult = NoteName.D }

          { Id = "e"
            Data = "e"
            ExpectedResult = NoteName.E }

          { Id = "f"
            Data = "f"
            ExpectedResult = NoteName.F }

          { Id = "g"
            Data = "g"
            ExpectedResult = NoteName.G }

          { Id = "a"
            Data = "a"
            ExpectedResult = NoteName.A }

          { Id = "b"
            Data = "b"
            ExpectedResult = NoteName.B }

          ]
    <| fun data expectedResult ->
        runWithStateAndAssert Parser.Functions.pNoteName parsingStateForTest data
        <| fun result _ -> result |> equal "Parsed note name is incorrect" expectedResult

// TODO: dotted durations
let ``parses a duration`` =
    tt
        "parses a duration"
        [

          { Id = "whole note"
            Data = "1"
            ExpectedResult = Duration.WholeNote }

          { Id = "half note"
            Data = "2"
            ExpectedResult = Duration.HalfNote }

          { Id = "quarter note"
            Data = "4"
            ExpectedResult = Duration.QuarterNote }

          { Id = "eighth note"
            Data = "8"
            ExpectedResult = Duration.EighthNote }

          { Id = "sixteenth note"
            Data = "16"
            ExpectedResult = Duration.SixteenthNote }

          ]
    <| fun data expectedResult ->
        runWithStateAndAssert Parser.Functions.pDuration parsingStateForTest data
        <| fun result _ -> result |> equal "Parsed duration is incorrect" expectedResult

// TODO: define octave rule
let ``parses a note`` =
    let aState lastNote =
        { InitialKeySignature = KeySignature NoteName.C
          InitialTimeSignature =
            { Numerator = 2
              Denominator = Duration.QuarterNote }
          InitialClef = Clef.G
          LastNote = lastNote
          LastMeasure = None }

    tt
        "parses a note"
        [

          { Id = "c8"
            Data = aState None, "c8"
            ExpectedResult =
              { NoteName = NoteName.C
                Octave = 4
                Duration = Duration.EighthNote } }

          { Id = "f16"
            Data = aState None, "f16"
            ExpectedResult =
              { NoteName = NoteName.F
                Octave = 4
                Duration = Duration.SixteenthNote } }

          { Id = "f, there is a last note ~~> uses last note duration"
            Data =
              aState (
                  Some
                      { NoteName = NoteName.C
                        Octave = 3
                        Duration = Duration.WholeNote }
              ),
              "f"
            ExpectedResult =
              { NoteName = NoteName.F
                Octave = 4
                Duration = Duration.WholeNote } }

          { Id = "f, there is not a last note ~~> uses current time signature denominator"
            Data = aState None, "f"
            ExpectedResult =
              { NoteName = NoteName.F
                Octave = 4
                Duration = Duration.QuarterNote } }

          { Id = "b1"
            Data = aState None, "b1"
            ExpectedResult =
              { NoteName = NoteName.B
                Octave = 4
                Duration = Duration.WholeNote } }

          ]
    <| fun (initialState, content) (expectedNote) ->
        runWithStateAndAssert Parser.Functions.pNote initialState content
        <| fun result finalState ->
            result |> equal "Parsed note is incorrect" expectedNote

            finalState.LastNote
            |> equal "Updated last note is incorrect" (Some expectedNote)

let ``parses sequences of notes`` =
    tt
        "parses sequences of notes"
        [

          { Id = "case 1"
            Data =
              { InitialTimeSignature =
                  { Numerator = 2
                    Denominator = Duration.QuarterNote }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                LastNote = None
                LastMeasure = None },
              openSample "sequence-of-notes-1.sls"
            ExpectedResult =
              let measure =
                  aMeasure
                  >> withClef Clef.G
                  >> withCNaturalKeySignature
                  >> withTimeSignature
                      { Numerator = 2
                        Denominator = Duration.QuarterNote }

              [ measure 1
                |> withNotes
                    [ { NoteName = NoteName.C
                        Octave = 4
                        Duration = Duration.EighthNote }

                      { NoteName = NoteName.D
                        Octave = 4
                        Duration = Duration.EighthNote }

                      { NoteName = NoteName.E
                        Octave = 4
                        Duration = Duration.EighthNote }

                      { NoteName = NoteName.D
                        Octave = 4
                        Duration = Duration.EighthNote } ]

                measure 2
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.HalfNote } ] }

          { Id = "case 2"
            Data =
              { InitialTimeSignature =
                  { Numerator = 3
                    Denominator = Duration.QuarterNote }
                InitialKeySignature = KeySignature NoteName.F
                InitialClef = Clef.F
                LastNote = None
                LastMeasure = None },
              openSample "sequence-of-notes-2.sls"
            ExpectedResult =
              let measure =
                  aMeasure
                  >> withClef Clef.F
                  >> withKeySignature (KeySignature NoteName.F)
                  >> withTimeSignature
                      { Numerator = 3
                        Denominator = Duration.QuarterNote }

              [ measure 1
                |> withNotes
                    [ { NoteName = NoteName.C
                        Octave = 4
                        Duration = Duration.QuarterNote }

                      { NoteName = NoteName.D
                        Octave = 4
                        Duration = Duration.QuarterNote }

                      { NoteName = NoteName.C
                        Octave = 4
                        Duration = Duration.QuarterNote } ]

                measure 2
                |> withNote
                    { NoteName = NoteName.F
                      Octave = 4
                      Duration = Duration.HalfNote }
                |> withNote
                    { NoteName = NoteName.G
                      Octave = 4
                      Duration = Duration.QuarterNote }

                measure 3
                |> withRepeteadNote
                    6
                    { NoteName = NoteName.E
                      Octave = 4
                      Duration = Duration.EighthNote }

                measure 4
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.HalfNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.QuarterNote } ] }

          { Id = "case 3"
            Data =
              { InitialTimeSignature =
                  { Numerator = 4
                    Denominator = Duration.QuarterNote }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                LastNote = None
                LastMeasure = None },
              openSample "sequence-of-notes-3.sls"
            ExpectedResult =
              let measure =
                  aMeasure
                  >> withClef Clef.G
                  >> withCNaturalKeySignature
                  >> withCommonTimeSignature

              [ measure 1
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.WholeNote }

                measure 2
                |> withNote
                    { NoteName = NoteName.G
                      Octave = 4
                      Duration = Duration.WholeNote }

                measure 3
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.WholeNote }

                measure 4
                |> withNote
                    { NoteName = NoteName.G
                      Octave = 4
                      Duration = Duration.WholeNote }

                measure 5
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.WholeNote } ] }

          ]
    <| fun (initialState, content) expectedResult ->
        runWithStateAndAssert Parser.Functions.pSequencesOfNotes initialState content
        <| fun result finalState -> result |> equal "Parsed duration is incorrect" expectedResult

[<Tests>]
let ParserSpec =
    testList
        "ParserSpec"
        [ ``parses music``
          ``parses a part definition``
          ``parses a note name``
          ``parses a duration``
          ``parses a note``
          ``parses sequences of notes`` ]
