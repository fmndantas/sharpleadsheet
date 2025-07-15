module Domain.MeasureBuilder

open Domain.GenericFunctions

open Domain.Types

type T = Measure

let emptyMeasure (measureNumber: MeasureNumber) : T =
    { MeasureNumber = measureNumber
      TimeSignature =
        { Numerator = 4
          Denominator = Duration.QuarterNote }
      KeySignature = KeySignature NoteName.C
      Notes = [] }

let withKeySignature (k: KeySignature) (m: T) : T = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: T) : T = { m with TimeSignature = t }

let withCNaturalKeySignature (m: T) : T =
    NoteName.C |> KeySignature |> (flip2 withKeySignature) m

let withCommonTimeSignature (m: T) : T =
    { Numerator = 4
      Denominator = Duration.QuarterNote }
    |> (flip2 withTimeSignature) m

let withNote (note: Note) (m: T) : T =
    { m with
        Notes = NoteOrPause.Note note :: m.Notes }
