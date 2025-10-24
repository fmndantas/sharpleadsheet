module Domain.MeasureBuilder

open Domain.GenericFunctions

open Domain.Types

let aMeasure (measureId: int) : UnvalidatedMeasure = {
  Clef = Clef.G
  Id = MeasureId measureId
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  NotesOrRests = []
}

let withKeySignature (k: KeySignature) (m: UnvalidatedMeasure) : UnvalidatedMeasure = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: UnvalidatedMeasure) : UnvalidatedMeasure = { m with TimeSignature = t }

let withCNaturalKeySignature (m: UnvalidatedMeasure) : UnvalidatedMeasure =
  NoteName.C |> KeySignature |> flip2 withKeySignature m

let withCommonTimeSignature (m: UnvalidatedMeasure) : UnvalidatedMeasure =
  {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  |> flip2 withTimeSignature m

let withClef (clef: Clef) (m: UnvalidatedMeasure) : UnvalidatedMeasure = { m with Clef = clef }

let withNote (note: Note.T) (m: UnvalidatedMeasure) : UnvalidatedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ NoteOrRest.Note note ]
}

let withRest (duration: Duration) (m: UnvalidatedMeasure) : UnvalidatedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ duration |> Rest |> NoteOrRest.Rest ]
}

let withNotes (notes: Note.T list) (m: UnvalidatedMeasure) : UnvalidatedMeasure = {
  m with
      NotesOrRests = List.map NoteOrRest.Note notes
}

let withRepeteadNote (count: int) (note: Note.T) (m: UnvalidatedMeasure) : UnvalidatedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests (List.replicate count (NoteOrRest.Note note))
}

let withSymbols (symbols: NoteOrRest list) (m: UnvalidatedMeasure) : UnvalidatedMeasure = { m with NotesOrRests = symbols }
