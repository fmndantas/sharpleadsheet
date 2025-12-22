module Domain.ParsedTypes

open CommonTypes

type ParsedMusic = {
  PartDefinitionSections: ParsedPartDefinitionSection list
  NotesSections: ParsedNotesSection list
}

and ParsedPartDefinitionSection = {
  Id: PartId option
  Name: string option
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  Clef: Clef
}

and ParsedNotesSection = {
  PartId: PartId
  Measures: ParsedMeasure list
}

and ParsedMeasure = {
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  Clef: Clef
  NotesOrRests: NoteOrRest list
}

type ParserState = {
  CurrentTimeSignature: TimeSignature
  CurrentKeySignature: KeySignature
  CurrentClef: Clef
  CurrentOctave: int
  LastPitch: Pitch.T option
  LastDuration: Duration.T option
  // TODO: mudar pra nome que implique no fato de que próxima nota ou pausa vai consumir e descartar
  LastChord: Chord.T option
}

type DefaultSettings = {
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  Clef: Clef
}
