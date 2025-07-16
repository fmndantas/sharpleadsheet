module Domain.Measure

open Domain.Types

open Operators

// TEST: Measure.generateEvents
let generateEvents (initialClef: Clef) (previousMeasure: Measure option) (currentMeasure: Measure) : MeasureEvent list =
    [ MeasureEvent.DefineKeySignature currentMeasure.KeySignature
      MeasureEvent.DefineTimeSignature currentMeasure.TimeSignature
      MeasureEvent.DefineClef initialClef

      yield!
          currentMeasure.Notes
          |> List.map (fun n ->
              match n with
              | NoteOrPause.Note note -> MeasureEvent.Note note) ]

let defineDivisions (measure: Measure) : int =
    if List.isEmpty measure.Notes then
        1
    else
        let minimalDuration =
            measure.Notes
            |> List.map NoteOrPause.getDuration
            |> List.minBy Duration.getEquivalenceToMinimalDuration

        match minimalDuration with
        | Duration.WholeNote
        | Duration.HalfNote
        | Duration.QuarterNote -> 1
        | Duration.EightNote -> 2
        | Duration.SixteenthNote -> 4
