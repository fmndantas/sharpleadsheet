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
  NotesOrRests = []
}

let withKeySignature (k: KeySignature) (m: Measure) : Measure = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: Measure) : Measure = { m with TimeSignature = t }

let withCNaturalKeySignature (m: Measure) : Measure =
  NoteName.C |> KeySignature |> flip2 withKeySignature m

let withCommonTimeSignature (m: Measure) : Measure =
  {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  |> flip2 withTimeSignature m

let withNote (note: Note.T) (m: Measure) : Measure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ NoteOrRest.Note note ]
}

let withRest (duration: Duration) (m: Measure) : Measure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ duration |> Rest |> NoteOrRest.Rest ]
}

let withNotes (notes: Note.T list) (m: Measure) : Measure = {
  m with
      NotesOrRests = List.map NoteOrRest.Note notes
}

let withSymbols (symbols: NoteOrRest list) (m: Measure) : Measure = { m with NotesOrRests = symbols }

let withRepeteadNote (count: int) (note: Note.T) (m: Measure) : Measure = {
  m with
      NotesOrRests = List.append m.NotesOrRests (List.replicate count (NoteOrRest.Note note))
}

let withClef (clef: Clef) (m: Measure) : Measure = { m with Clef = clef }
