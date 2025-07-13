module Domain.MusicToXml

open System.Xml.Linq

open Domain.XmlWrapper

open Domain.Types

type PartId = PartId of int

let partId2String (PartId partId) = $"P{partId}"

let indexWithPartId (xs: 'a list) : list<PartId * 'a> =
    xs |> List.indexed |> List.map (fun (idx, x) -> idx + 1 |> PartId, x)

let createPartList (names: Part list) : XElement =
    names
    |> indexWithPartId
    |> List.map (fun (partId, part) ->
        elementWithAttributes
            "score-part"
            [ partId |> partId2String |> attribute "id" ]
            [ leafElement "part-name" part.Name ])
    |> element "part-list"

let measureNumber2String (MeasureNumber measureNumber) = measureNumber.ToString()

// TEST: calculateFifths
let calculateFifths (k: KeySignature) : string = "0"

// TEST: calculateBeatType
let calculateBeatType (t: TimeSignature) : string = "4"

// TEST: interpretClefEvent
let interpretClefEvent (c: Clef) : XElement =
    [ leafElement "sign" "G"; leafElement "line" "2" ] |> element "clef"

// TEST: defineDivisions
let defineDivisions (m: Measure) : int = 1

let interpretMeasureEvents (m: Measure) (es: MeasureEvent list) : XElement =
    [ m |> defineDivisions |> _.ToString() |> leafElement "divisions"
      yield!
          es
          |> List.map (fun e ->
              match e with
              | MeasureEvent.DefineKeySignature k -> element "key" [ k |> calculateFifths |> leafElement "fifths" ]
              | MeasureEvent.DefineTimeSignature t ->
                  element
                      "time"
                      [ leafElement "beats" (t.Numerator.ToString())
                        t |> calculateBeatType |> leafElement "beat-type" ]
              | MeasureEvent.DefineClef c -> interpretClefEvent c) ]
    |> element "attributes"

// TEST: interpretNoteEvents
let interpretNoteEvents (es: NoteEvent list) : XElement list =
    es
    |> List.map (fun e ->
        match e with
        | NoteEvent.Note note ->
            [ element "pitch" [ leafElement "step" "C"; leafElement "octave" "4" ]
              leafElement "duration" "4"
              leafElement "type" "whole" ]
        | NoteEvent.Pause pause -> failwith "todo"
        |> element "note")

let createMeasure (initialClef: Clef) (previousMeasure: Measure option, currentMeasure: Measure) : XElement =
    [ Measure.generateEvents initialClef previousMeasure currentMeasure
      |> interpretMeasureEvents currentMeasure
      yield! currentMeasure |> Note.generateEvents |> interpretNoteEvents ]
    |> elementWithAttributes "measure" [ currentMeasure.MeasureNumber |> measureNumber2String |> attribute "number" ]

let createPart (ps: Part list) : XElement list =
    ps
    |> indexWithPartId
    |> List.map (fun (partId, part) ->
        let measures = part.Measures

        let pairsOfMeasures =
            if List.isEmpty measures then
                []
            else
                (None, List.head measures)
                :: (measures |> List.pairwise |> List.map (fun (a, b) -> Some a, b))

        pairsOfMeasures
        |> List.map (createMeasure part.Clef)
        |> elementWithAttributes "part" [ partId |> partId2String |> attribute "id" ])

// TODO: add validation
let convert (m: Music) : XDocument =
    let (Music parts) = m

    [ parts |> createPartList; yield! createPart parts ]
    |> elementWithAttributes "score-partwise" [ attribute "version" "4.0" ]
    |> document
