module Domain.MeasureBuilder

open Domain.GenericFunctions

open Domain.Types

let aParsedMeasure () : ParsedMeasure = {
  Clef = Clef.G
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  NotesOrRests = []
}

let withKeySignature (k: KeySignature) (m: ParsedMeasure) : ParsedMeasure = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: ParsedMeasure) : ParsedMeasure = { m with TimeSignature = t }

let withCNaturalKeySignature (m: ParsedMeasure) : ParsedMeasure =
  NoteName.C |> KeySignature |> flip2 withKeySignature m

let withCommonTimeSignature (m: ParsedMeasure) : ParsedMeasure =
  {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  |> flip2 withTimeSignature m

let withClef (clef: Clef) (m: ParsedMeasure) : ParsedMeasure = { m with Clef = clef }

let withNote (note: Note.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ NoteOrRest.Note note ]
}

let withRest (duration: Duration) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ duration |> Rest |> NoteOrRest.Rest ]
}

let withNotes (notes: Note.T list) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.map NoteOrRest.Note notes
}

let withRepeteadNote (count: int) (note: Note.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests (List.replicate count (NoteOrRest.Note note))
}

let withSymbols (symbols: NoteOrRest list) (m: ParsedMeasure) : ParsedMeasure = { m with NotesOrRests = symbols }

let toValidatedMeasure (id: int) (m: ParsedMeasure) : ValidatedMeasure = { Id = MeasureId id; Parsed = m }
