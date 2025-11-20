module Domain.MusicToXml

open System.Xml.Linq

open Domain.XmlWrapper

open Domain.Types

let partId2String (PartId partId) = $"P{partId}"

let indexWithPartId (xs: 'a list) : list<PartId * 'a> =
  xs |> List.indexed |> List.map (fun (idx, x) -> idx + 1 |> PartId, x)

let createPartList (names: ValidatedPart list) : XElement =
  names
  |> indexWithPartId
  |> List.map (fun (partId, part) ->
    elementWithAttributes "score-part" [ partId |> partId2String |> attribute "id" ] [
      leafElement "part-name" part.Name
    ])
  |> element "part-list"

let measureId2String (MeasureId measureId) = measureId.ToString()

let calculateFifths =
  KeySignature.fifths
  >> function
    | Fifth.Zero -> "0"
    | Fifth.Flat flat -> $"{-flat}"
    | Fifth.Sharp sharp -> $"{sharp}"

// TEST: calculateBeatType
let calculateBeatType (t: TimeSignature) : string = "4"

// TEST: interpretClefEvent
let interpretClefEvent (c: Clef) : XElement =
  [ leafElement "sign" "G"; leafElement "line" "2" ] |> element "clef"

// TEST: interpretNote
let interpretNote (n: NoteOrRest) : XElement =
  [
    element "pitch" [ leafElement "step" "C"; leafElement "octave" "4" ]
    leafElement "duration" "4"
    leafElement "type" "whole"
  ]
  |> element "note"

let createMeasureAttributes (m: ValidatedMeasure) (es: MeasureEvent list) : XElement =
  [
    m |> Measure.defineDivisions |> _.ToString() |> leafElement "divisions"
    yield!
      es
      |> List.choose (fun e ->
        match e with
        | MeasureEvent.DefineKeySignature k -> element "key" [ k |> calculateFifths |> leafElement "fifths" ] |> Some
        | MeasureEvent.DefineTimeSignature t ->
          element "time" [
            leafElement "beats" (t.Numerator.ToString())
            t |> calculateBeatType |> leafElement "beat-type"
          ]
          |> Some
        | MeasureEvent.DefineClef c -> interpretClefEvent >> Some <| c
        | _ -> None)
  ]
  |> element "attributes"

let createMeasureNotes (es: MeasureEvent list) : XElement list =
  es
  |> List.choose (fun e ->
    match e with
    | MeasureEvent.NoteOrRest noteOrRest -> interpretNote noteOrRest |> Some
    | _ -> None)

let createMeasure (previousMeasure: ValidatedMeasure option, currentMeasure: ValidatedMeasure) : XElement =
  let events = Measure.generateEvents previousMeasure currentMeasure

  [
    createMeasureAttributes currentMeasure events
    yield! createMeasureNotes events
  ]
  |> elementWithAttributes "measure" [ currentMeasure.MeasureId |> measureId2String |> attribute "number" ]

let createPart (ps: ValidatedPart list) : XElement list =
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
    |> List.map createMeasure
    |> elementWithAttributes "part" [ partId |> partId2String |> attribute "id" ])

// TODO: add validation
let convert (m: Music) : XDocument =
  let (Music.Validated parts) = m

  [ parts |> createPartList; yield! createPart parts ]
  |> elementWithAttributes "score-partwise" [ attribute "version" "4.0" ]
  |> document
