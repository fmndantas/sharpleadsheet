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
      LastMeasureId = None
      LastNote = None }

let ``parses a part definition section`` =
    testTheory2
        "parses a part definition section"
        [ { Id = "case 1"
            Data = openSample "part-definition-1.sls"
            ExpectedResult =
              { Id = PartId 1 |> Some
                Name = Some "Piano"
                Clef = Some Clef.G
                TimeSignature =
                  Some
                      { Numerator = 2
                        Denominator = Duration.Quarter }
                KeySignature = KeySignature NoteName.F |> Some } }

          { Id = "case 2"
            Data = openSample "part-definition-2.sls"
            ExpectedResult =
              { Id = PartId 1 |> Some
                Name = Some "guitar"
                Clef = Some Clef.G
                TimeSignature =
                  Some
                      { Numerator = 1
                        Denominator = Duration.Eighth }
                KeySignature = KeySignature NoteName.G |> Some } } ]
    <| fun content expectedResult ->
        runWithStateAndAssert Parser.Functions.pPartDefinitionSection parsingStateForTest content
        <| fun result _ -> result |> equal "Part definition section is incorrect" expectedResult

// TODO: sharp and flat notes
let ``parses a note name`` =
    testTheory2
        "parses a note name"
        [ { Id = "c"
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
            ExpectedResult = NoteName.B } ]
    <| fun data expectedResult ->
        runWithStateAndAssert Parser.Functions.pNoteName parsingStateForTest data
        <| fun result _ -> result |> equal "Note name is incorrect" expectedResult

// TODO: dotted durations
let ``parses a duration`` =
    testTheory2
        "parses a duration"
        [ { Id = "whole note"
            Data = "1"
            ExpectedResult = Duration.Whole }

          { Id = "half note"
            Data = "2"
            ExpectedResult = Duration.Half }

          { Id = "quarter note"
            Data = "4"
            ExpectedResult = Duration.Quarter }

          { Id = "eighth note"
            Data = "8"
            ExpectedResult = Duration.Eighth }

          { Id = "sixteenth note"
            Data = "16"
            ExpectedResult = Duration.Sixteenth } ]
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
          LastNote = lastNote
          LastMeasureId = None }

    testTheory2
        "parses a note"
        [ { Id = "c8"
            Data = aState None, "c8"
            ExpectedResult =
              { NoteName = NoteName.C
                Octave = 4
                Duration = Duration.Eighth } }

          { Id = "f16"
            Data = aState None, "f16"
            ExpectedResult =
              { NoteName = NoteName.F
                Octave = 4
                Duration = Duration.Sixteenth } }

          { Id = "f, there is a last note ~~> uses last note duration"
            Data =
              aState (
                  Some
                      { NoteName = NoteName.C
                        Octave = 3
                        Duration = Duration.Whole }
              ),
              "f"
            ExpectedResult =
              { NoteName = NoteName.F
                Octave = 4
                Duration = Duration.Whole } }

          { Id = "f, there is not a last note ~~> uses current time signature denominator"
            Data = aState None, "f"
            ExpectedResult =
              { NoteName = NoteName.F
                Octave = 4
                Duration = Duration.Quarter } }

          { Id = "b1"
            Data = aState None, "b1"
            ExpectedResult =
              { NoteName = NoteName.B
                Octave = 4
                Duration = Duration.Whole } } ]
    <| fun (currentState, content) expectedResult ->
        runWithStateAndAssert Parser.Functions.pNote currentState content
        <| fun result finalState ->
            result |> equal "Note is incorrect" expectedResult

            finalState.LastNote
            |> equal "Updated last note is incorrect" (Some expectedResult)

let ``parses sequences of notes`` =
    testTheory2
        "parses sequences of notes"
        [ { Id = "case 1"
            Data =
              { InitialTimeSignature =
                  { Numerator = 2
                    Denominator = Duration.Quarter }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                LastNote = None
                LastMeasureId = None },
              openSample "sequence-of-notes-1.sls"
            ExpectedResult =
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
                      Duration = Duration.Half } ] }

          { Id = "case 2"
            Data =
              { InitialTimeSignature =
                  { Numerator = 3
                    Denominator = Duration.Quarter }
                InitialKeySignature = KeySignature NoteName.F
                InitialClef = Clef.F
                LastNote = None
                LastMeasureId = None },
              openSample "sequence-of-notes-2.sls"
            ExpectedResult =
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
                      Duration = Duration.Quarter } ] }

          { Id = "case 3"
            Data =
              { InitialTimeSignature =
                  { Numerator = 4
                    Denominator = Duration.Quarter }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                LastNote = None
                LastMeasureId = None },
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
                      Duration = Duration.Whole } ] }

          { Id = "case 4"
            Data =
              { InitialTimeSignature =
                  { Numerator = 4
                    Denominator = Duration.Quarter }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                LastNote = None
                LastMeasureId = None },
              openSample "sequence-of-notes-4.sls"
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
                      Duration = Duration.Whole }

                measure 2
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.Whole } ] }

          { Id = "case 5"
            Data =
              { InitialTimeSignature =
                  { Numerator = 4
                    Denominator = Duration.Quarter }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                CurrentOctave = 4
                LastNote = None
                LastMeasureId = None },
              openSample "sequence-of-notes-5.sls"
            ExpectedResult =
              let measure =
                  aMeasure
                  >> withCommonTimeSignature
                  >> withCNaturalKeySignature
                  >> withClef Clef.G

              [

                measure 1
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.Half }
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 5
                      Duration = Duration.Half }


                ] } ]
    <| fun (currentState, content) expectedResult ->
        runWithStateAndAssert Parser.Functions.pSequencesOfNotes currentState content
        <| fun result finalState -> result |> equal "Sequence of notes is incorrect" expectedResult

let ``parses notes section`` =
    testTheory2
        "parses notes section"
        [ { Id = "case 1"
            Data =
              { InitialTimeSignature =
                  { Numerator = 4
                    Denominator = Duration.Quarter }
                InitialKeySignature = KeySignature NoteName.C
                InitialClef = Clef.G
                LastNote = None
                LastMeasureId = None },
              openSample "notes-section-1.sls"
            ExpectedResult =
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
                          Duration = Duration.Whole } ] },
              {| LastMeasureId = MeasureId 2 |> Some
                 LastNote =
                  Some
                      { NoteName = NoteName.C
                        Octave = 4
                        Duration = Duration.Whole } |} } ]
    <| fun (currentState, content) (expectedResult, expectedFinalState) ->
        runWithStateAndAssert Parser.Functions.pNotesSection currentState content
        <| fun result finalState ->
            result |> equal "Notes section is incorrect" expectedResult

            {| LastMeasureId = finalState.LastMeasureId
               LastNote = finalState.LastNote |}
            |> equal "Final state is incorrect" expectedFinalState

let ``parses music`` =
    testTheory2
        "parses music"
        [ { Id = "case 1"
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
                                Duration = Duration.Half } ] } ] }

          { Id = "case 2"
            Data = openSample "example-2.sls"
            ExpectedResult =
              Music
                  [ { Id = PartId 1
                      Name = "bass"
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
                                Duration = Duration.Eighth } ] } ] } ]
    <| fun (fileContent) (expectedResult: Music) ->
        runWithStateAndAssert Parser.Functions.pMusic parsingStateForTest fileContent
        <| fun result _ -> result |> equal "Music is incorrect" expectedResult

[<Tests>]
let ParserSpec =
    testList
        "ParserSpec"
        [ ``parses a part definition section``
          ``parses a note name``
          ``parses a duration``
          ``parses a note``
          ``parses sequences of notes``
          ``parses notes section``
          ``parses music`` ]
