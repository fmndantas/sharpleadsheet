module Domain.MusicToXml

open System.Xml.Linq

open XmlWrapper

open CommonTypes
open Measure.Types

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

let calculateBeatType (t: TimeSignature) : string =
  match t.Denominator with
  | Duration.Whole -> 1
  | Duration.Half -> 2
  | Duration.Quarter -> 4
  | Duration.Eighth -> 8
  | Duration.Sixteenth -> 16
  | Duration.ThirtySecond -> 32
  | _ -> failwith "unsupported duration for beat type"
  |> toString

let interpretClefEvent (c: Clef) : XElement =
  let sign, line =
    match c with
    | Clef.G -> "G", "2"
    | Clef.F -> "F", "4"

  [ leafElement "sign" sign; leafElement "line" line ] |> element "clef"

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

let interpretNote
  (divisions: Duration.T)
  ({
     NoteOrRest = noteOrRest
     AttachedToNoteOrRestEvents = attachedToNoteOrRestEvents
   }: NoteOrRestEvent)
  : XElement =
  let xmlTie =
    fun xmlType -> [
      elementWithAttributes "tie" [ attribute "type" xmlType ] []
      element "notations" [ elementWithAttributes "tied" [ attribute "type" xmlType ] [] ]
    ]

  let duration = NoteOrRest.getDuration noteOrRest

  element "note" [
    match noteOrRest with
    | NoteOrRest.Rest _ -> selfEnclosingElement "rest"
    | NoteOrRest.Note note -> yield! [ note |> Note.getPitch |> interpretPitch ]
    yield! interpretDuration divisions duration
    if attachedToNoteOrRestEvents |> List.contains StartTie then
      yield! xmlTie "start"
    if attachedToNoteOrRestEvents |> List.contains StopTie then
      yield! xmlTie "stop"
  ]

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
        | DefineKeySignatureEvent k -> element "key" [ k |> calculateFifths |> leafElement "fifths" ] |> Some
        | DefineTimeSignatureEvent t ->
          element "time" [
            leafElement "beats" (t.Numerator.ToString())
            t |> calculateBeatType |> leafElement "beat-type"
          ]
          |> Some
        | DefineClefEvent c -> c |> interpretClefEvent |> Some
        | _ -> None)
  ]
  |> element "attributes"

let createMeasureNotes (m: Validated.Measure) (es: MeasureEvent list) : XElement list =
  es
  |> List.choose (fun e ->
    match e with
    | NoteOrRestEvent noteOrRestEvent -> (Measure.defineDivisions m, noteOrRestEvent) ||> interpretNote |> Some
    | _ -> None)

let createMeasure (m: Validated.Measure, es: MeasureEvent list) : XElement =
  [ createMeasureAttributes m es; yield! createMeasureNotes m es ]
  |> elementWithAttributes "measure" [ m.MeasureId |> measureId2String |> attribute "number" ]

let createPart (ps: Validated.Part list) : XElement list =
  ps
  |> List.map (fun part ->
    part.Measures
    |> List.mapFold Measure.generateEvents {
      IsFirstMeasure = true
      IsTieStarted = false
      CurrentKeySignature = part.KeySignature
      CurrentTimeSignature = part.TimeSignature
      CurrentClef = part.Clef
    }
    |> fst
    |> List.zip part.Measures
    |> List.map createMeasure
    |> elementWithAttributes "part" [ part.PartId |> partId2String |> attribute "id" ])

let convert (m: Validated.Music) : XDocument =
  [ m |> createPartList; yield! createPart m ]
  |> elementWithAttributes "score-partwise" [ attribute "version" "4.0" ]
  |> document
