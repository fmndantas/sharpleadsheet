module Domain.MusicToXml

open System.Xml.Linq

open Domain.XmlWrapper

open Domain.Types

type PartId = PartId of int

let partId2String (PartId partId) = $"P{partId}"

let indexWithPartId (xs: 'a list) : list<PartId * 'a> =
    xs |> List.indexed |> List.map (fun (idx, x) -> idx + 1 |> PartId, x)

let createPartList (names: string list) : XElement =
    names
    |> indexWithPartId
    |> List.map (fun (partId, name) ->
        elementWithAttributes
            "score-part"
            [ partId |> partId2String |> attribute "id" ]
            [ leafElement "part-name" name ])
    |> elementWithAttributes "part-list" []

let measureNumber2String (MeasureNumber measureNumber) = measureNumber.ToString()

// TEST: calculateFifths
let calculateFifths (k: KeySignature) : string = "0"

// TEST: calculateBeatType
let calculateBeatType (t: TimeSignature) : string = "4"

let interpretMeasureEvents (es: MeasureEvent list) : XElement =
    es
    |> List.map (fun e ->
        match e with
        | MeasureEvent.DefineKeySignature k -> element "key" [ k |> calculateFifths |> leafElement "fifths" ]
        | MeasureEvent.DefineTimeSignature t ->
            elementWithAttributes
                "time"
                []
                [ leafElement "beats" (t.Numerator.ToString())
                  t |> calculateBeatType |> leafElement "beat-type" ])
    |> elementWithAttributes "attributes" []

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

let createMeasure (previous: Measure option, current: Measure) : XElement =
    [ (previous, current) ||> Measure.generateEvents |> interpretMeasureEvents
      yield! current |> Note.generateEvents |> interpretNoteEvents ]
    |> elementWithAttributes "measure" [ current.MeasureNumber |> measureNumber2String |> attribute "number" ]

let createPart (measures: Measure list list) : XElement list =
    measures
    |> indexWithPartId
    |> List.map (fun (partId, measures) ->
        let pairsOfMeasures =
            if List.isEmpty measures then
                []
            else
                List.append
                    [ None, List.head measures ]
                    (measures |> List.pairwise |> List.map (fun (a, b) -> Some a, b))

        pairsOfMeasures
        |> List.map createMeasure
        |> elementWithAttributes "part" [ partId |> partId2String |> attribute "id" ])

// TODO: add validation
let convert (m: Music) : XDocument =
    let (Music parts) = m

    [ parts |> List.map _.Name |> createPartList
      yield! parts |> List.map _.Measures |> createPart ]
    |> elementWithAttributes "score-partwise" [ attribute "version" "4.0" ]
    |> document
