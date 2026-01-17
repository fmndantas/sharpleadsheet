module Domain.ParsedTypes

open CommonTypes

type ParsedModifier = { Prefix: string; Content: string }

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
  // TODO: change LastChord to another name that implies it'll be consumed by next note or rest
  LastChord: Chord.T option
  LastText: string option
}

type DefaultSettings = {
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  Clef: Clef
}
