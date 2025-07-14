module UnitTests.MusicToXmlSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types
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
                        // TODO: use measure builder here
                        [ { MeasureNumber = MeasureNumber 1
                            TimeSignature =
                              { Numerator = 4
                                Denominator = Duration.QuarterNote }
                            KeySignature =
                              { NaturalNote = NaturalNote.C
                                Accidental = Accidental.Natural }
                            Notes =
                              [ NoteEvent.Note
                                    { NaturalNote = NaturalNote.C
                                      Accidental = Accidental.Natural
                                      Octave = 3
                                      Duration = Duration.WholeNote } ] } ] } ]
            ExpectedResult = expectedResult } ]
            ExpectedResult = openXml "helloworld.xml" } ]
    <| fun (music: Music) (expectedResult: string) ->
        let result = convert music

        (XmlWrapper.normalizeXml result, XmlWrapper.normalizeXmlText expectedResult)
        ||> equal "Generated XML is incorrect"

[<Tests>]
let MusicToXmlSpec =
    testList "MusicToXmlSpec" [ ``it should convert music to xml`` ]
