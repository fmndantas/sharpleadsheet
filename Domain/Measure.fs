module Domain.Measure

open Domain.CommonTypes

open Operators

let generateEvents
  (previousMeasure: Validated.Measure option)
  ({ Parsed = currentMeasure }: Validated.Measure)
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
