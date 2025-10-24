module Domain.Measure

open Domain.Types

open Operators

let generateEvents
  (previousMeasure: UnvalidatedMeasure option)
  ({ Parsed = currentMeasure }: UnvalidatedMeasure)
  : MeasureEvent list =
  [
    yield!
      previousMeasure
      |> Option.map (fun { Parsed = p } -> [
        if p.KeySignature <> currentMeasure.KeySignature then
          MeasureEvent.DefineKeySignature currentMeasure.KeySignature

        if p.TimeSignature <> currentMeasure.TimeSignature then
          MeasureEvent.DefineTimeSignature currentMeasure.TimeSignature

        if p.Clef <> currentMeasure.Clef then
          MeasureEvent.DefineClef currentMeasure.Clef
      ])
      |> Option.defaultValue [
        MeasureEvent.DefineKeySignature currentMeasure.KeySignature
        MeasureEvent.DefineTimeSignature currentMeasure.TimeSignature
        MeasureEvent.DefineClef currentMeasure.Clef
      ]
    yield! List.map MeasureEvent.NoteOrRest currentMeasure.NotesOrRests
  ]

let defineDivisions ({ Parsed = measure }: UnvalidatedMeasure) : int =
  if List.isEmpty measure.NotesOrRests then
    1
  else
    let minimalDuration =
      measure.NotesOrRests
      |> List.map NoteOrRest.getDuration
      |> List.minBy Duration.getEquivalenceToMinimalDuration

    match minimalDuration with
    | Duration.Whole
    | Duration.Half
    | Duration.Quarter -> 1
    | Duration.Eighth -> 2
    | Duration.Sixteenth -> 4
