module Domain.Measure

open Domain.Types

// TEST: Measure.generateEvents
let generateEvents (initialClef: Clef) (previousMeasure: Measure option) (currentMeasure: Measure) : MeasureEvent list =
    [ MeasureEvent.DefineKeySignature currentMeasure.KeySignature
      MeasureEvent.DefineTimeSignature currentMeasure.TimeSignature
      MeasureEvent.DefineClef initialClef ]
