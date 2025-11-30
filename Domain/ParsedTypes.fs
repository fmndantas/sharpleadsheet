module Domain.ParsedTypes

open Domain.CommonTypes

type ParsedMusic = {
  PartDefinitionSections: ParsedPartDefinitionSection list
  NotesSections: ParsedNotesSection list
}

and ParsedPartDefinitionSection = {
  Id: PartId option
  Name: string option
  Clef: Clef option
  TimeSignature: TimeSignature option
  KeySignature: KeySignature option
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
