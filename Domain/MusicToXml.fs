module Domain.MusicToXml

open System.Xml.Linq

open Domain.XmlWrapper

open Domain.CommonTypes

let partId2String (PartId partId) = $"P{partId}"

let createPartList (names: Validated.Part list) : XElement =
  names
  |> List.map (fun part ->
    elementWithAttributes "score-part" [ part.PartId |> partId2String |> attribute "id" ] [
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

let interpretDuration (divisions: Duration.T) (d: Duration.T) : XElement list =
  let duration =
    match Duration.getEquivalenceToMinimalDuration divisions, Duration.getEquivalenceToMinimalDuration d with
    | Duration.Equivalence.Multiple den, Duration.Equivalence.Multiple num -> num / den

  let durationType =
    match d with
    | Duration.Whole -> "whole"
    | Duration.WholeDotted -> "whole"
    | Duration.Half -> "half"
    | Duration.HalfDotted -> "half"
    | Duration.Quarter -> "quarter"
    | Duration.QuarterDotted -> "quarter"
    | Duration.Eighth -> "eighth"
    | Duration.EighthDotted -> "eighth"
    | Duration.Sixteenth -> "16th"
    | Duration.SixteenthDotted -> "16th"
    | Duration.ThirtySecond -> "32nd"

  let isDotted = d.ToString().ToLower().Contains "dotted"

  [
    leafElement "duration" (duration.ToString())
    leafElement "type" durationType
    if isDotted then
      selfEnclosingElement "dot"
  ]

let interpretPitch (p: Pitch.T) : XElement =
  let noteName = Pitch.getNoteName p
  let stringNoteName = noteName.ToString()
  let octave = Pitch.getOctave p
  let step = noteName.ToString()[0]

  let alter =
    let lowerStringNoteName = stringNoteName.ToLower()

    if lowerStringNoteName.Contains "flat" then -1
    elif lowerStringNoteName.Contains "sharp" then 1
    else 0

  element "pitch" [
    leafElement "step" (step.ToString())
    leafElement "octave" (octave.ToString())
    if alter <> 0 then
      leafElement "alter" (sprintf "%+d" alter)
  ]

let interpretNote (divisions: Duration.T) (n: NoteOrRest) : XElement =
  match n with
  | NoteOrRest.Note note ->
    [
      note |> Note.getPitch |> interpretPitch
      yield! n |> NoteOrRest.getDuration |> interpretDuration divisions
    ]
    |> element "note"
  | NoteOrRest.Rest(Rest d) -> d |> interpretDuration divisions |> element "rest"

let createMeasureAttributes (m: Validated.Measure) (es: MeasureEvent list) : XElement =
  [
    m
    |> Measure.defineDivisions
    |> function
      | Duration.Quarter -> 1
      | Duration.Eighth -> 2
      | Duration.Sixteenth -> 4
      | Duration.ThirtySecond -> 8
      | _ -> failwith "unsupported duration for divisions"
    |> _.ToString()
    |> leafElement "divisions"
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
        | MeasureEvent.DefineClef c -> c |> interpretClefEvent |> Some
        | _ -> None)
  ]
  |> element "attributes"

let createMeasureNotes (m: Validated.Measure) (es: MeasureEvent list) : XElement list =
  es
  |> List.choose (fun e ->
    match e with
    | MeasureEvent.NoteOrRest noteOrRest -> (Measure.defineDivisions m, noteOrRest) ||> interpretNote |> Some
    | _ -> None)

let createMeasure (previousMeasure: Validated.Measure option, currentMeasure: Validated.Measure) : XElement =
  let events = Measure.generateEvents previousMeasure currentMeasure

  [
    createMeasureAttributes currentMeasure events
    yield! createMeasureNotes currentMeasure events
  ]
  |> elementWithAttributes "measure" [ currentMeasure.MeasureId |> measureId2String |> attribute "number" ]

let createPart (ps: Validated.Part list) : XElement list =
  ps
  |> List.map (fun part ->
    let measures = part.Measures

    let pairsOfMeasures =
      if List.isEmpty measures then
        []
      else
        (None, List.head measures)
        :: (measures |> List.pairwise |> List.map (fun (a, b) -> Some a, b))

    pairsOfMeasures
    |> List.map createMeasure
    |> elementWithAttributes "part" [ part.PartId |> partId2String |> attribute "id" ])

let convert (m: Validated.Music) : XDocument =
  [ m |> createPartList; yield! createPart m ]
  |> elementWithAttributes "score-partwise" [ attribute "version" "4.0" ]
  |> document
