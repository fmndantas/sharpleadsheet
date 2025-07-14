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
        { NaturalNote = NaturalNote.C
          Accidental = Accidental.Natural }
      Notes = [] }

let withKeySignature (k: KeySignature) (m: T) : T = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: T) : T = { m with TimeSignature = t }

let cNatural (m: T) : T =
    { NaturalNote = NaturalNote.C
      Accidental = Accidental.Natural }
    |> (flip2 withKeySignature) m

let commonTime (m: T) : T =
    { Numerator = 4
      Denominator = Duration.QuarterNote }
    |> (flip2 withTimeSignature) m

let withNote (note: Note) (m: T) : T =
    { m with
        Notes = NoteOrPause.Note note :: m.Notes }
