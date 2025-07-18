module UnitTests.ParserSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open Domain.Types
open Domain.MeasureBuilder

[<Literal>]
let here = __SOURCE_DIRECTORY__

let openSharpLeadsheet (file: string) =
    let dot = Directory.GetParent(here).FullName
    File.ReadAllText(Path.Join(dot, "Samples", file))

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
                                Denominator = Duration.HalfNote }
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
                                Denominator = Duration.HalfNote }

                          ] } ] }

          ]
    <| fun (fileContent) (expectedResult: Music) ->
        let result = Parser.parse fileContent

        result
        |> wantOk "Parsing failed"
        |> equal "Parsed music is incorrect" expectedResult

[<Tests>]
let ParserSpec = testList "ParserSpec" [ ``parses music`` ]
