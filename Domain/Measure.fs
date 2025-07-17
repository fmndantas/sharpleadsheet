module Domain.Measure

open Domain.Types

open Operators

let generateEvents (previousMeasure: Measure option) (currentMeasure: Measure) : MeasureEvent list =
    [ yield!
          previousMeasure
          |> Option.map (fun p ->
              [ if p.KeySignature <> currentMeasure.KeySignature then
                    MeasureEvent.DefineKeySignature currentMeasure.KeySignature

                if p.TimeSignature <> currentMeasure.TimeSignature then
                    MeasureEvent.DefineTimeSignature currentMeasure.TimeSignature

                if p.Clef <> currentMeasure.Clef then
                    MeasureEvent.DefineClef currentMeasure.Clef ])
          |> Option.defaultValue
              [ MeasureEvent.DefineKeySignature currentMeasure.KeySignature
                MeasureEvent.DefineTimeSignature currentMeasure.TimeSignature
                MeasureEvent.DefineClef currentMeasure.Clef ]
      yield! List.map MeasureEvent.NoteOrPause currentMeasure.Notes ]

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
