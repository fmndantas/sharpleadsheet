module Domain.Measure

open Domain.CommonTypes

open Operators

module Types =
  type MeasureContext = {
    IsFirstMeasure: bool
    IsTieStarted: bool
    CurrentKeySignature: KeySignature
    CurrentTimeSignature: TimeSignature
    CurrentClef: Clef
  }

  type MeasureEvent =
    | DefineKeySignatureEvent of KeySignature
    | DefineTimeSignatureEvent of TimeSignature
    | DefineClefEvent of Clef
    | NoteOrRestEvent of NoteOrRestEvent

  and NoteOrRestEvent = {
    NoteOrRest: NoteOrRest
    AttachedToNoteOrRestEvents: AttachedToNoteOrRestEvent list
  }

  and AttachedToNoteOrRestEvent =
    | StartTie
    | StopTie

open Types

module CreateEvent =
  let noteOrRestEventWithExtra (extra: AttachedToNoteOrRestEvent list) (noteOrRest: NoteOrRest) : MeasureEvent =
    NoteOrRestEvent {
      NoteOrRest = noteOrRest
      AttachedToNoteOrRestEvents = extra
    }

  let noteOrRestEvent (noteOrRest: NoteOrRest) : MeasureEvent = noteOrRestEventWithExtra [] noteOrRest

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
  ]

  let updatedContextAfterMeasureStart = {
    context with
        IsFirstMeasure = false
        CurrentKeySignature = measure.KeySignature
        CurrentTimeSignature = measure.TimeSignature
        CurrentClef = measure.Clef
  }

  let noteOrRestEvents, updatedContextAfterNotesProcessing =
    measure.NotesOrRests
    |> List.mapFold
      (fun context noteOrRest ->
        let isNoteStartingATie = NoteOrRest.isTied noteOrRest
        let isNoteEndingATie = context.IsTieStarted

        noteOrRest
        |> CreateEvent.noteOrRestEventWithExtra [
          if isNoteStartingATie then
            StartTie
          if isNoteEndingATie then
            StopTie
        ],
        {
          context with
              IsTieStarted = isNoteStartingATie
        })
      updatedContextAfterMeasureStart

  List.concat [ otherEvents; noteOrRestEvents ], updatedContextAfterNotesProcessing

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
