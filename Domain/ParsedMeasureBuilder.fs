module Domain.ParsedMeasureBuilder

open GenericFunctions

open CommonTypes
open ParsedTypes

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
      NotesOrRests = List.append m.NotesOrRests [ NoteOrRest.fromNote note ]
}

let withRest (rest: Rest.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ NoteOrRest.fromRest rest ]
}

let withNotes (notes: Note.T list) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = [ yield! m.NotesOrRests; yield! List.map NoteOrRest.fromNote notes ]
}

let withRepeteadNote (count: int) (note: Note.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests (List.replicate count (NoteOrRest.fromNote note))
}

let withRepeatedRest (count: int) (rest: Rest.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests (List.replicate count (NoteOrRest.fromRest rest))
}

// TODO: "Symbol" or "Symbols" are good names?
let withSymbol (symbol: NoteOrRest.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests [ symbol ]
}

let withSymbols (symbols: NoteOrRest.T list) (m: ParsedMeasure) : ParsedMeasure = { m with NotesOrRests = symbols }

let withRepeteadSymbols (count: int) (symbol: NoteOrRest.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      NotesOrRests = List.append m.NotesOrRests (List.replicate count symbol)
}
