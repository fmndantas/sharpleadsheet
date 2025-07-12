module Domain.MeasureBuilder

open Domain.GenericFunctions

open Domain.Types

type T = Measure

let emptyMeasure (measureNumber: MeasureNumber) : T =
    { MeasureNumber = measureNumber
      TimeSignature =
        { Numerator = 4
          Denominator = Duration.QuarterNote }
      KeySignature =
        KeySignature
            { NaturalNote = NaturalNote.C
              Accidental = Accidental.Natural }
      NoteEvents = []
      ChordEvents = [] }

let withKeySignature (k: KeySignature) (m: T) : T = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: T) : T = { m with TimeSignature = t }

let cNatural (m: T) : T =
    { NaturalNote = NaturalNote.C
      Accidental = Accidental.Natural }
    |> KeySignature
    |> (flip2 withKeySignature) m

let commonTime (m: T) : T =
    { Numerator = 4
      Denominator = Duration.QuarterNote }
    |> (flip2 withTimeSignature) m
