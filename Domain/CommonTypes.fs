module Domain.CommonTypes

type MeasureId = MeasureId of int
type PartId = PartId of int

type Rest = Rest of Duration.T

type TimeSignature = {
  Numerator: int
  Denominator: Duration.T
}

type KeySignature = KeySignature of NoteName.T

[<RequireQualifiedAccess>]
type NoteOrRest =
  | Note of Note.T
  | Rest of Rest

[<RequireQualifiedAccess>]
type Clef =
  | G
  | F

[<RequireQualifiedAccess>]
type ValidationError =
  | PartDefinitionMissingName of index: int
  | PartDefinitionMissingId of index: int
  | PartDefinitionsWithRepeatedIds of PartsWithRepeatedIds
  | NotesSectionReferencesInvalidPartId of NotesSectionReferencesInvalidPartId
  | MeasureWithInconsistentDurations of (MeasureId * PartId)

and PartsWithRepeatedIds = { PartId: PartId; Indexes: int list }
and NotesSectionReferencesInvalidPartId = { PartId: PartId; Index: int }

[<RequireQualifiedAccess>]
type MeasureEvent =
  | DefineKeySignature of KeySignature
  | DefineTimeSignature of TimeSignature
  | DefineClef of Clef
  | NoteOrRest of NoteOrRest

[<RequireQualifiedAccess>]
type Fifth =
  | Zero
  | Flat of int
  | Sharp of int
