module Domain.Measure

open CommonTypes

open GenericFunctions

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
    | VoiceEntryEvent of VoiceEntryEvent
    | FinalBarlineEvent

  and VoiceEntryEvent = {
    VoiceEntry: VoiceEntry.T
    EventsAttachedToVoiceEntry: EventAttachedToVoiceEntry list
  }

  and EventAttachedToVoiceEntry = | StopTie

open Types

module Event =
  let voiceEntry (n: VoiceEntry.T) : MeasureEvent =
    VoiceEntryEvent {
      VoiceEntry = n
      EventsAttachedToVoiceEntry = []
    }

  let withStopTie (e: MeasureEvent) : MeasureEvent =
    match e with
    | VoiceEntryEvent e ->
      VoiceEntryEvent {
        e with
            EventsAttachedToVoiceEntry = StopTie :: e.EventsAttachedToVoiceEntry
      }
    | other -> other

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

  let voiceEntryEvents, context'' =
    measure.VoiceEntries
    |> List.mapFold
      (fun context voiceEntry ->
        let isNoteStartingATie = VoiceEntry.isTied voiceEntry
        let isNoteEndingATie = context.IsTieStarted

        voiceEntry
        |> Event.voiceEntry
        |> modifyIfTrue isNoteEndingATie Event.withStopTie,
        {
          context with
              IsTieStarted = isNoteStartingATie
        })
      context'

  List.concat [ otherEvents; voiceEntryEvents ], context''

let defineDivisions ({ Parsed = measure }: Validated.Measure) : Duration.T =
  if List.isEmpty measure.VoiceEntries then
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

    measure.VoiceEntries
    |> List.map (VoiceEntry.getDuration >> dottedToStraight)
    |> List.minBy Duration.getEquivalenceToMinimalDuration
    |> function
      | Duration.Whole
      | Duration.Half
      | Duration.Quarter -> Duration.Quarter
      | v -> v
