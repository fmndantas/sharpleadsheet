module Domain.MeasureBuilder

open Domain.GenericFunctions

open Domain.Types

let aMeasure (measureId: int) : Measure = {
  Clef = Clef.G
  Id = MeasureId measureId
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  Notes = []
}

let withKeySignature (k: KeySignature) (m: Measure) : Measure = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: Measure) : Measure = { m with TimeSignature = t }

let withCNaturalKeySignature (m: Measure) : Measure =
  NoteName.C |> KeySignature |> (flip2 withKeySignature) m

let withCommonTimeSignature (m: Measure) : Measure =
  {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  |> (flip2 withTimeSignature) m

let withNote (note: Note) (m: Measure) : Measure = {
  m with
      Notes = List.append m.Notes [ NoteOrPause.Note note ]
}

let withNotes (notes: Note list) (m: Measure) : Measure = {
  m with
      Notes = List.map NoteOrPause.Note notes
}

let withRepeteadNote (count: int) (note: Note) (m: Measure) : Measure = {
  m with
      Notes = List.append m.Notes (List.replicate count (NoteOrPause.Note note))
}

let withClef (clef: Clef) (m: Measure) : Measure = { m with Clef = clef }
