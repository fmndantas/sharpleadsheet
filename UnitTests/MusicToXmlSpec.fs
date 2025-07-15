module UnitTests.MusicToXmlSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types
open Domain.MeasureBuilder
open Domain.MusicToXml

[<Literal>]
let here = __SOURCE_DIRECTORY__

let openXml (file: string) =
    File.ReadAllText(Path.Join(here, "Xmls", file))

let ``it should convert music to xml`` =
    tt
        "it should convert music to xml"
        [ { Id = "simplest case possible"
            Data =
              Music
                  [ { Name = "Instrument Name"
                      Clef = Clef.G
                      Measures =
                        [ emptyMeasure (MeasureNumber 1)
                          |> withCommonTimeSignature
                          |> withCNaturalKeySignature
                          |> withNote
                              { NoteName = NoteName.C
                                Octave = 4
                                Duration = Duration.WholeNote } ] }

                    ]
            ExpectedResult = openXml "helloworld.xml" } ]
    <| fun (music: Music) (expectedResult: string) ->
        let result = convert music

        (XmlWrapper.normalizeXmlText expectedResult, XmlWrapper.normalizeXml result)
        ||> equal "Generated XML is incorrect"

[<Tests>]
let MusicToXmlSpec =
    testList "MusicToXmlSpec" [ ``it should convert music to xml`` ]
