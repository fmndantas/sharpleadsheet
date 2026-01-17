module UnitTests.MusicToXmlSpec

open System
open System.IO

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open CommonTypes
open ParsedMeasureBuilder
open Validated
open ValidatedMeasureBuilder
open Measure.Types

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
            Clef = Clef.G
            TimeSignature = {
              Numerator = 4
              Denominator = Duration.Quarter
            }
            KeySignature = KeySignature NoteName.C
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

    (XmlWrapper.minifyXDocumentText expectedResult, XmlWrapper.minifyXDocument result)
    ||> equal "generated xml is incorrect"

let ``converts note or rest to xml`` =
  testTheory3 "converts note or rest to xml" [
    caseId(1)
      .WithData(
        Duration.Quarter,
        Note.create4 NoteName.C Duration.Whole
        |> NoteOrRest.Note
        |> Measure.CreateEvent.noteOrRestEvent
      )
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

    caseId(2)
      .WithData(
        Duration.Quarter,
        Duration.Quarter
        |> Rest.create
        |> NoteOrRest.Rest
        |> Measure.CreateEvent.noteOrRestEvent
      )
      .WithExpectedResult(
        "
      <note>
        <rest/>
        <duration>1</duration>
        <type>quarter</type>
      </note>
      "
      )

    caseId(3)
      .WithData(
        Duration.Sixteenth,
        Note.create4 NoteName.FSharp Duration.EighthDotted
        |> NoteOrRest.Note
        |> Measure.CreateEvent.noteOrRestEvent
      )
      .WithExpectedResult(
        "
      <note>
        <pitch>
          <step>F</step>
          <octave>4</octave>
          <alter>+1</alter>
        </pitch>
        <duration>3</duration>
        <type>eighth</type>
        <dot/>
      </note>
      "
      )

    case("4.tie start")
      .WithData(
        Duration.Quarter,
        Note.createTied4 NoteName.C Duration.Quarter
        |> NoteOrRest.Note
        |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StartTie ]
      )
      .WithExpectedResult(
        "
      <note>
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <duration>1</duration>
        <type>quarter</type>
        <tie type=\"start\"/>
        <notations>
          <tied type=\"start\"/>
        </notations>
      </note>
      "
      )

    case("5.tie stop")
      .WithData(
        Duration.Quarter,
        Note.create4 NoteName.C Duration.Quarter
        |> NoteOrRest.Note
        |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ StopTie ]
      )
      .WithExpectedResult(
        "
      <note>
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <duration>1</duration>
        <type>quarter</type>
        <tie type=\"stop\"/>
        <notations>
          <tied type=\"stop\"/>
        </notations>
      </note>
      "
      )

    case("6.chord attached to note")
      .WithData(
        Duration.Quarter,
        Note.create4 NoteName.C Duration.Whole
        |> Note.withChord (Chord.createWithBassAndKind NoteName.CSharp NoteName.GFlat "maj9(#11/13)")
        |> NoteOrRest.Note
        |> Measure.CreateEvent.noteOrRestEvent
      )
      .WithExpectedResult
      "
        <harmony>
          <root>
            <root-step>C</root-step>
            <root-alter>+1</root-alter>
          </root>
          <bass>
            <bass-step>G</bass-step>
            <bass-alter>-1</bass-alter>
          </bass>
          <kind>maj9(#11/13)</kind>
        </harmony>
        <note>
          <pitch>
            <step>C</step>
            <octave>4</octave>
          </pitch>
          <duration>4</duration>
          <type>whole</type>
        </note>
        "

    case("7.chord attached to rest")
      .WithData(
        Duration.Quarter,
        Rest.create Duration.Whole
        |> Rest.withChord (Chord.createWithBass NoteName.BFlat NoteName.FSharp)
        |> NoteOrRest.Rest
        |> Measure.CreateEvent.noteOrRestEvent
      )
      .WithExpectedResult
      "
        <harmony>
          <root>
            <root-step>B</root-step>
            <root-alter>-1</root-alter>
          </root>
          <bass>
            <bass-step>F</bass-step>
            <bass-alter>+1</bass-alter>
          </bass>
        </harmony>
        <note>
          <rest/>
          <duration>4</duration>
          <type>whole</type>
        </note>
        "

    case("8.text attached to note")
      .WithData(
        Duration.Quarter,
        (NoteName.C, Duration.Whole)
        ||> Note.create4
        |> Note.withText "text attached to note"
        |> NoteOrRest.Note
        |> Measure.CreateEvent.noteOrRestEventWithAttachedEvents [ Text "text attached to note" ]
      )
      .WithExpectedResult
      "
      <direction placement=\"above\">
        <direction-type>
          <words>text attached to note</words>
        </direction-type>
      </direction>
      <note>
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <duration>4</duration>
        <type>whole</type>
      </note>
      "
  ]
  <| fun (divisions, noteOrRestEventAsMeasureEvent) expectedResult ->
    let noteOrRestEvent =
      match noteOrRestEventAsMeasureEvent with
      | NoteOrRestEvent e -> e
      | _ -> failwith "expected NoteOrRestEvent"

    let result =
      (divisions, noteOrRestEvent)
      ||> MusicToXml.interpretNoteOrRest
      |> XmlWrapper.element "dummyWrapper"
      |> XmlWrapper.minifyXElement

    let expectedResult' = "<dummyWrapper>" + expectedResult + "</dummyWrapper>"

    (XmlWrapper.minifyXDocumentText expectedResult', result)
    ||> equal "generated xml is incorrect"

let ``converts duration to xml`` =
  testTheory3 "converts duration to xml" [
    caseId(1).WithData(Duration.Quarter, Duration.Quarter).WithExpectedResult(1, "quarter", false)
    caseId(2).WithData(Duration.Quarter, Duration.Whole).WithExpectedResult(4, "whole", false)
    caseId(3).WithData(Duration.Sixteenth, Duration.Quarter).WithExpectedResult(4, "quarter", false)
    caseId(4).WithData(Duration.ThirtySecond, Duration.Quarter).WithExpectedResult(8, "quarter", false)
    caseId(5).WithData(Duration.ThirtySecond, Duration.WholeDotted).WithExpectedResult(48, "whole", true)
    caseId(6).WithData(Duration.ThirtySecond, Duration.QuarterDotted).WithExpectedResult(12, "quarter", true)
    caseId(7).WithData(Duration.ThirtySecond, Duration.ThirtySecond).WithExpectedResult(1, "32nd", false)
    caseId(8).WithData(Duration.ThirtySecond, Duration.SixteenthDotted).WithExpectedResult(3, "16th", true)
  ]
  <| fun (divisions, duration) (expectedDuration, expectedType, hasDot) ->
    let minifiedResult =
      (divisions, duration)
      ||> MusicToXml.interpretDuration
      |> List.map XmlWrapper.minifyXElement

    [
      ("duration", toString expectedDuration) ||> XmlWrapper.leafElement
      XmlWrapper.leafElement "type" expectedType
      if hasDot then
        XmlWrapper.selfEnclosingElement "dot"
    ]
    |> List.indexed
    |> List.iter (fun (i, item) ->
      contains (sprintf "item %d not found" i) (XmlWrapper.minifyXElement item) minifiedResult)

let ``converts pitch to xml`` =
  testTheory3 "converts pitch to xml" [
    caseId(1).WithData(Pitch.create NoteName.C 4).WithExpectedResult("C", 4, 0)
    caseId(2).WithData(Pitch.create NoteName.CSharp 4).WithExpectedResult("C", 4, 1)
    caseId(3).WithData(Pitch.create NoteName.DFlat 4).WithExpectedResult("D", 4, -1)
    caseId(4).WithData(Pitch.create NoteName.D 3).WithExpectedResult("D", 3, 0)
    caseId(5).WithData(Pitch.create NoteName.DSharp 5).WithExpectedResult("D", 5, 1)
    caseId(6).WithData(Pitch.create NoteName.EFlat 4).WithExpectedResult("E", 4, -1)
    caseId(7).WithData(Pitch.create NoteName.E 4).WithExpectedResult("E", 4, 0)
    caseId(8).WithData(Pitch.create NoteName.F 4).WithExpectedResult("F", 4, 0)
    caseId(9).WithData(Pitch.create NoteName.FSharp 4).WithExpectedResult("F", 4, 1)
    caseId(10).WithData(Pitch.create NoteName.G 4).WithExpectedResult("G", 4, 0)
    caseId(11).WithData(Pitch.create NoteName.GSharp 4).WithExpectedResult("G", 4, 1)
    caseId(12).WithData(Pitch.create NoteName.AFlat 4).WithExpectedResult("A", 4, -1)
    caseId(13).WithData(Pitch.create NoteName.A 4).WithExpectedResult("A", 4, 0)
    caseId(14).WithData(Pitch.create NoteName.ASharp 4).WithExpectedResult("A", 4, 1)
    caseId(15).WithData(Pitch.create NoteName.BFlat 4).WithExpectedResult("B", 4, -1)
    caseId(16).WithData(Pitch.create NoteName.B 4).WithExpectedResult("B", 4, 0)
  ]
  <| fun pitch (step, octave, alter) ->
    pitch
    |> MusicToXml.interpretPitch
    |> XmlWrapper.minifyXElement
    |> equal
      "generated xml is incorrect"
      (sprintf
        "<pitch><step>%s</step><octave>%d</octave>%s</pitch>"
        step
        octave
        (if alter = 0 then
           String.Empty
         else
           sprintf "<alter>%+d</alter>" alter))

[<Tests>]
let MusicToXmlSpec =
  testList "music to xml" [
    ``converts music to xml``
    ``converts note or rest to xml``
    ``converts duration to xml``
    ``converts pitch to xml``
  ]
