module UnitTests.MusicToXmlSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.CommonTypes
open Domain.ParsedMeasureBuilder
open Domain.Validated
open Domain.ValidatedMeasureBuilder

[<Literal>]
let here = __SOURCE_DIRECTORY__

let openXml (file: string) =
  File.ReadAllText(Path.Join(here, "Xmls", file))

let ``converts music to xml`` =
  testTheory3 "converts music to xml" [
    case("hello world")
      .WithData(
        [
          {
            Name = "Instrument Name"
            PartId = PartId 10
            Measures = [
              aParsedMeasure ()
              |> withClef Clef.G
              |> withCommonTimeSignature
              |> withCNaturalKeySignature
              |> withNote (Note.create4 NoteName.C Duration.Whole)
              |> toValidatedMeasure 1
            ]
          }
        ]
      )
      .WithExpectedResult(openXml "helloworld.xml")
  ]
  <| fun (music: Validated.Music) (expectedResult: string) ->
    let result = MusicToXml.convert music

    (XmlWrapper.normalizeXmlText expectedResult, XmlWrapper.normalizeXml result)
    ||> equal "generated XML is incorrect"

let ``converts note or rest to xml`` =
  testTheory3 "converts note or rest to xml" [
    caseId(1)
      .WithData(Note.create4 NoteName.C Duration.Whole |> NoteOrRest.Note)
      .WithExpectedResult(
        "
        <note>
          <pitch>
            <step>C</step>
            <octave>4</octave>
          </pitch>
          <duration>4</duration>
          <type>whole</type>
        </note>
      "
      )
  ]
  <| fun noteOrRest expectedResult ->
    let result = MusicToXml.interpretNote noteOrRest

    (XmlWrapper.normalizeXmlText expectedResult, XmlWrapper.minifyXElement result)
    ||> equal "generated XML is incorrect"

[<Tests>]
let MusicToXmlSpec =
  testList "music to xml" [ ``converts music to xml``; ``converts note or rest to xml`` ]
