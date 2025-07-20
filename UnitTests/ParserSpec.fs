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

let private openSharpLeadsheet (file: string) =
    let dot = Directory.GetParent(here).FullName
    File.ReadAllText(Path.Join(dot, "Samples", file))

let private runAndAssert p content assertFn =
    match run p content with
    | Success(result, _, _) -> assertFn result
    | Failure(errorMessage, _, _) -> failtest errorMessage

let ``parses music`` =
    tt
        "parses music"
        [

          { Id = "one measure"
            Data = openSharpLeadsheet "one-measure.sls"
            ExpectedResult =
              Music
                  [ { Name = "Piano"
                      Id = PartId 1
                      Measures =
                        [

                          aMeasure 1
                          |> withCNaturalKeySignature
                          |> withTimeSignature
                              { Numerator = 2
                                Denominator = Duration.QuarterNote }
                          |> withNote
                              { NoteName = NoteName.C
                                Octave = 4
                                Duration = Duration.EightNote }
                          |> withNote
                              { NoteName = NoteName.D
                                Octave = 4
                                Duration = Duration.EightNote }
                          |> withNote
                              { NoteName = NoteName.E
                                Octave = 4
                                Duration = Duration.EightNote }
                          |> withNote
                              { NoteName = NoteName.D
                                Octave = 4
                                Duration = Duration.EightNote }

                          aMeasure 2
                          |> withCNaturalKeySignature
                          |> withTimeSignature
                              { Numerator = 2
                                Denominator = Duration.QuarterNote }
                          |> withNote
                              { NoteName = NoteName.C
                                Octave = 4
                                Duration = Duration.HalfNote }

                          ] } ] }

          ]
    <| fun (fileContent) (expectedResult: Music) ->
        runAndAssert Parser.Functions.pMusic fileContent (equal "Parsed music is incorrect" expectedResult)

let ``parses a part definition`` =
    testCase "parses a part definition"
    <| fun () ->
        let part = openSharpLeadsheet "part-definition.sls"

        let expectedResult =
            { Id = PartId 1 |> Some
              Name = Some "Piano"
              Clef = Some Clef.G
              TimeSignature =
                Some
                    { Numerator = 2
                      Denominator = Duration.QuarterNote }
              KeySignature = KeySignature NoteName.F |> Some }

        runAndAssert Parser.Functions.pPartDefinition part (fun result ->
            result |> equal "Part definition is incorrect" expectedResult)

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
        runAndAssert Parser.Functions.Helpers.pNoteName data (equal "Parsed note name is incorrect" expectedResult)

[<Tests>]
let ParserSpec =
    testList "ParserSpec" [ ``parses music``; ``parses a part definition``; ``parses a note name`` ]
