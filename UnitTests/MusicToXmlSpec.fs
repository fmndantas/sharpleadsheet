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

let ``converts music to xml`` =
  testTheory2 "converts music to xml" [
    {
      Id = "hello world"
      Data =
        Unvalidated [
          {
            Name = "Instrument Name"
            Id = PartId 1
            Measures = [
              aParsedMeasure ()
              |> withClef Clef.G
              |> withCommonTimeSignature
              |> withCNaturalKeySignature
              |> withNote (Note.create4 NoteName.C Duration.Whole)
              |> toUnvalidatedMeasure 1
            ]
          }
        ]
      ExpectedResult = openXml "helloworld.xml"
    }
  ]
  <| fun (music: Music) (expectedResult: string) ->
    let result = convert music

    (XmlWrapper.normalizeXmlText expectedResult, XmlWrapper.normalizeXml result)
    ||> equal "Generated XML is incorrect"

[<Tests>]
let MusicToXmlSpec = testList "MusicToXmlSpec" [ ``converts music to xml`` ]
