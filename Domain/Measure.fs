module Domain.Measure

open CommonTypes

open Operators

module Types =
  type MeasureContext = {
    IsFirstMeasure: bool
    IsTieStarted: bool
    CurrentKeySignature: KeySignature
    CurrentTimeSignature: TimeSignature
    CurrentClef: Clef
    TotalNumberOfMeasures: int
    CurrentMeasureIndex: int
  }

  type MeasureEvent =
    | DefineKeySignatureEvent of KeySignature
    | DefineTimeSignatureEvent of TimeSignature
    | DefineClefEvent of Clef
    | NoteOrRestEvent of NoteOrRestEvent
    | FinalBarlineEvent

  and NoteOrRestEvent = {
    NoteOrRest: NoteOrRest.T
    AttachedToNoteOrRestEvents: AttachedToNoteOrRestEvent list
  }

  // TODO: think about have events here that don't make sense in Validated.Music entities
  // for example: StartTie can be expressed with Note/Rest.Modifier.Tie and StartTie is not needed
  // StopTie not; reason: it does make sense only in musicxml context
  // SetChord can be expressed as Note/Rest.Modifier.Chord
  // This idea is reinforced by the fact that I'm feeling duplication evil in unit tests:
  // Note does not need to have modifier and but the test will pass (because interpret note uses measure events)
  and AttachedToNoteOrRestEvent =
    | StartTie
    | StopTie
    | SetChord of Chord.T
    | Text of string

open Types

// TODO: idea: make this module more builder like
// example: note |> withStartTie |> with...
// CreateEvent -> Event
// noteOrRestEvent
//   -> note
//   -> rest
// delete noteOrRestEventWithAttachedEvents
module CreateEvent =
  let noteOrRestEventWithAttachedEvents (attached: AttachedToNoteOrRestEvent list) (n: NoteOrRest.T) : MeasureEvent =
    NoteOrRestEvent {
      NoteOrRest = n
      AttachedToNoteOrRestEvents = attached
    }

  let noteOrRestEvent (n: NoteOrRest.T) : MeasureEvent = noteOrRestEventWithAttachedEvents [] n

let generateEvents
  (context: MeasureContext)
  ({ Parsed = measure }: Validated.Measure)
  : MeasureEvent list * MeasureContext =

  let otherEvents = [
    if context.IsFirstMeasure then
      DefineKeySignatureEvent measure.KeySignature
      DefineTimeSignatureEvent measure.TimeSignature
      DefineClefEvent measure.Clef

    if context.CurrentKeySignature <> measure.KeySignature then
      DefineKeySignatureEvent measure.KeySignature

    if context.CurrentTimeSignature <> measure.TimeSignature then
      DefineTimeSignatureEvent measure.TimeSignature

    if context.CurrentClef <> measure.Clef then
      DefineClefEvent measure.Clef

    if context.TotalNumberOfMeasures = context.CurrentMeasureIndex + 1 then
      FinalBarlineEvent
  ]

  let context' = {
    context with
        IsFirstMeasure = false
        CurrentKeySignature = measure.KeySignature
        CurrentTimeSignature = measure.TimeSignature
        CurrentClef = measure.Clef
        CurrentMeasureIndex = context.CurrentMeasureIndex + 1
  }

  let noteOrRestEvents, context'' =
    measure.NotesOrRests
    |> List.mapFold
      (fun context noteOrRest ->
        let isNoteStartingATie = NoteOrRest.isTied noteOrRest
        let isNoteEndingATie = context.IsTieStarted

        noteOrRest
        |> CreateEvent.noteOrRestEventWithAttachedEvents [
          if isNoteStartingATie then
            StartTie
          if isNoteEndingATie then
            StopTie
          yield! noteOrRest |> NoteOrRest.getText |> Option.map Text |> Option.toList
        ],
        {
          context with
              IsTieStarted = isNoteStartingATie
        })
      context'

  List.concat [ otherEvents; noteOrRestEvents ], context''

let defineDivisions ({ Parsed = measure }: Validated.Measure) : Duration.T =
  if List.isEmpty measure.NotesOrRests then
    Duration.Quarter
  else
    let dottedToStraight =
      function
      | Duration.WholeDotted -> Duration.Half
      | Duration.HalfDotted -> Duration.Quarter
      | Duration.QuarterDotted -> Duration.Eighth
      | Duration.EighthDotted -> Duration.Sixteenth
      | Duration.SixteenthDotted -> Duration.ThirtySecond
      | v -> v

    measure.NotesOrRests
    |> List.map (NoteOrRest.getDuration >> dottedToStraight)
    |> List.minBy Duration.getEquivalenceToMinimalDuration
    |> function
      | Duration.Whole
      | Duration.Half
      | Duration.Quarter -> Duration.Quarter
      | v -> v
