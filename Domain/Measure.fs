module Domain.Measure

open Domain.Types

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
