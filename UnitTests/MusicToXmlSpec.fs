module UnitTests.MusicToXmlSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain.Types
open Domain.MusicToXml

let expectedResult =
    """
<score-partwise version="4.0">
  <part-list>
    <score-part id="P1">
      <part-name>P1</part-name>
    </score-part>
  </part-list>
  <part id="P1">
    <measure number="1">
      <attributes>
        <divisions>1</divisions>
        <key>
          <fifths>0</fifths>
        </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
        </time>
        <clef>
          <sign>G</sign>
          <line>2</line>
        </clef>
      </attributes>
      <note>
        <rest/>
        <duration>4</duration>
        <voice>1</voice>
        <type>whole</type>
      </note>
      <barline>
        <bar-style>light-heavy</bar-style>
      </barline>
    </measure>
  </part>
</score-partwise>
"""

let ``it should convert music to xml`` =
    tt
        "it should convert music to xml"
        [ { Id = "simplest case possible"
            Data =
              Music
                  [ { Name = "Instrument name"
                      Measures =
                        // TODO: use measure builder here
                        [ { MeasureNumber = MeasureNumber 1
                            TimeSignature =
                              { Numerator = 4
                                Denominator = Duration.QuarterNote }
                            KeySignature =
                              KeySignature
                                  { NaturalNote = NaturalNote.C
                                    Accidental = Accidental.Natural }
                            NoteEvents = []
                            ChordEvents = [] } ] } ]
            ExpectedResult = expectedResult } ]
    <| fun (music: Music) (expectedResult: string) ->
        let result = convert music
        result.ToString() |> equal "Generated XML is incorrect" expectedResult

[<Tests>]
let MusicToXmlSpec =
    testList "MusicToXmlSpec" [ ``it should convert music to xml`` ]
