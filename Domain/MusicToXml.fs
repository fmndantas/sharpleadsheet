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

let measureId2String (MeasureId measureId) = toString measureId

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

  let isDotted = d |> toString |> (fun v -> v.ToLower().Contains "dotted")

  [
    leafElement "duration" (toString duration)
    leafElement "type" durationType
    if isDotted then
      selfEnclosingElement "dot"
  ]

let private noteNameToStep (tagName: string) (n: NoteName.T) : XElement =
  let value =
    let v = n |> toString
    v[0] |> toString

  leafElement tagName value

let private noteNameToAlter (tagName: string) (n: NoteName.T) : XElement option =
  let lowerStringNoteName = n |> toString |> _.ToLower()

  (if lowerStringNoteName.Contains "flat" then Some -1
   elif lowerStringNoteName.Contains "sharp" then Some 1
   else None)
  |> Option.map (sprintf "%+d" >> leafElement tagName)

let interpretPitch (p: Pitch.T) : XElement =
  let noteName = Pitch.getNoteName p
  let octave = Pitch.getOctave p

  element "pitch" [
    p |> Pitch.getNoteName |> noteNameToStep "step"
    octave |> toString |> leafElement "octave"
    yield! noteName |> noteNameToAlter "alter" |> Option.toList
  ]

let private interpretChordRoot (chord: Chord.T) : XElement =
  let root = Chord.getRoot chord

  [
    noteNameToStep "root-step" root
    yield! root |> noteNameToAlter "root-alter" |> Option.toList
  ]
  |> element "root"

let private interpretChordBass (chord: Chord.T) : XElement option =
  let maybeBass = Chord.getBass chord

  maybeBass
  |> Option.map (fun bass ->
    [
      noteNameToStep "bass-step" bass
      yield! bass |> noteNameToAlter "bass-alter" |> Option.toList
    ]
    |> element "bass")

let private interpretChord (chord: Chord.T) : XElement =
  [
    chord |> interpretChordRoot
    yield! chord |> interpretChordBass |> Option.toList
    yield! chord |> Chord.getKind |> Option.map (leafElement "kind") |> Option.toList
  ]
  |> element "harmony"

let interpretNoteOrRest
  (divisions: Duration.T)
  ({
     NoteOrRest = noteOrRest
     AttachedToNoteOrRestEvents = attachedToNoteOrRestEvents
   }: NoteOrRestEvent)
  : XElement list =
  let xmlTie xmlType = [
    selfEnclosingElementWithAttributes "tie" [ attribute "type" xmlType ]
    element "notations" [ selfEnclosingElementWithAttributes "tied" [ attribute "type" xmlType ] ]
  ]

  let duration = NoteOrRest.getDuration noteOrRest

  let text =
    attachedToNoteOrRestEvents
    |> List.tryFind _.IsText
    |> Option.bind (function
      | Text t ->
        elementWithAttributes "direction" [ attribute "placement" "above" ] [
          element "direction-type" [ leafElement "words" t ]
        ]
        |> Some
      | _ -> None)

  // TODO: chord does not use measure event!
  let chord = noteOrRest |> NoteOrRest.getChord |> Option.map interpretChord

  let note =
    element "note" [
      NoteOrRest.fold (Note.getPitch >> interpretPitch) (fun _ -> selfEnclosingElement "rest") noteOrRest
      yield! interpretDuration divisions duration
      if attachedToNoteOrRestEvents |> List.contains StartTie then
        yield! xmlTie "start"
      if attachedToNoteOrRestEvents |> List.contains StopTie then
        yield! xmlTie "stop"
    ]

  [ yield! Option.toList chord; yield! Option.toList text; note ]

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
    |> toString
    |> leafElement "divisions"
    yield!
      es
      |> List.choose (fun e ->
        match e with
        | DefineKeySignatureEvent k -> element "key" [ k |> calculateFifths |> leafElement "fifths" ] |> Some
        | DefineTimeSignatureEvent t ->
          element "time" [
            leafElement "beats" (toString t.Numerator)
            t |> calculateBeatType |> leafElement "beat-type"
          ]
          |> Some
        | DefineClefEvent c -> c |> interpretClefEvent |> Some
        | _ -> None)
  ]
  |> element "attributes"

let createMeasureNotes (m: Validated.Measure) (es: MeasureEvent list) : XElement list =
  es
  |> List.collect (fun e ->
    match e with
    | NoteOrRestEvent noteOrRestEvent -> (Measure.defineDivisions m, noteOrRestEvent) ||> interpretNoteOrRest
    | _ -> [])

let private createFinalBarline (es: MeasureEvent list) : XElement option =
  if List.contains FinalBarlineEvent es then
    elementWithAttributes "barline" [ attribute "location" "right" ] [ leafElement "bar-style" "light-heavy" ]
    |> Some
  else
    None

let createMeasure (m: Validated.Measure, es: MeasureEvent list) : XElement =
  [
    createMeasureAttributes m es
    yield! createMeasureNotes m es
    yield! es |> createFinalBarline |> Option.toList
  ]
  |> elementWithAttributes "measure" [ m.MeasureId |> measureId2String |> attribute "number" ]

let createParts (ps: Validated.Part list) : XElement list =
  ps
  |> List.map (fun part ->
    part.Measures
    |> List.mapFold Measure.generateEvents {
      IsFirstMeasure = true
      IsTieStarted = false
      CurrentKeySignature = part.KeySignature
      CurrentTimeSignature = part.TimeSignature
      CurrentClef = part.Clef
      TotalNumberOfMeasures = List.length part.Measures
      CurrentMeasureIndex = 0
    }
    |> fst
    |> List.zip part.Measures
    |> List.map createMeasure
    |> elementWithAttributes "part" [ part.PartId |> partId2String |> attribute "id" ])

let convert (m: Validated.Music) : XDocument =
  [ m |> createPartList; yield! createParts m ]
  |> elementWithAttributes "score-partwise" [ attribute "version" "4.0" ]
  |> document
