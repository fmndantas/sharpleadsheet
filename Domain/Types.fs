module Domain.Types

[<RequireQualifiedAccess>]
module Duration =
  type T =
    | Whole
    | WholeDotted
    | Half
    | HalfDotted
    | Quarter
    | QuarterDotted
    | Eighth
    | EighthDotted
    | Sixteenth
    | SixteenthDotted

  type Equivalence =
    | Multiple of int
    | Divider of int

  let private durations = [ Whole; Half; Quarter; Eighth; Sixteenth ]
  let private minimalDuration = List.last durations

  let getEquivalence (unitOfEquivalence: T) (targetDuration: T) : Equivalence =
    let u = durations |> List.findIndex ((=) unitOfEquivalence) |> pown 2
    let t = durations |> List.findIndex ((=) targetDuration) |> pown 2
    if u >= t then u / t |> Multiple else t / u |> Divider

  let getEquivalenceToMinimalDuration = getEquivalence minimalDuration

[<RequireQualifiedAccess>]
type NoteName =
  | C
  | CSharp
  | DFlat
  | D
  | DSharp
  | EFlat
  | E
  | F
  | FSharp
  | GFlat
  | G
  | GSharp
  | AFlat
  | A
  | ASharp
  | BFlat
  | B

module Pitch =
  type T = private {
    NoteName: NoteName
    Octave: int
  }

  let create noteName octave = { NoteName = noteName; Octave = octave }

  let createMiddle noteName = create noteName 4

module Note =
  type T = private {
    Pitch: Pitch.T
    Duration: Duration.T
    Modifiers: Modifier list
  }

  and Modifier = | Tie

  /// Create a note without modifiers
  let create octave noteName duration = {
    Pitch = Pitch.create noteName octave
    Duration = duration
    Modifiers = []
  }

  /// Create a note with modifiers
  let create' octave modifiers noteName duration = {
    Pitch = Pitch.create noteName octave
    Duration = duration
    Modifiers = modifiers
  }

  let create2 = create 2

  let create4 = create 4

  let create4' = create' 4

  let createTied4 = create4' [ Tie ]

  let create5 = create 5

  let create6 = create 6

  let getPitch note = note.Pitch

  let getDuration note = note.Duration

type Rest = Rest of Duration.T

type TimeSignature = {
  Numerator: int
  Denominator: Duration.T
}

type KeySignature = KeySignature of NoteName

type MeasureId = MeasureId of int
type PartId = PartId of int

[<RequireQualifiedAccess>]
type NoteOrRest =
  | Note of Note.T
  | Rest of Rest

[<RequireQualifiedAccess>]
type Clef =
  | G
  | F

type ParsedPartDefinitionSection = {
  Id: PartId option
  Name: string option
  Clef: Clef option
  TimeSignature: TimeSignature option
  KeySignature: KeySignature option
}

type ParsedMeasure = {
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  Clef: Clef
  NotesOrRests: NoteOrRest list
}

type ParsedNotesSection = {
  PartId: PartId
  Measures: ParsedMeasure list
}

type ParsedMusic = {
  PartDefinitionSections: ParsedPartDefinitionSection list
  NotesSections: ParsedNotesSection list
}

[<RequireQualifiedAccess>]
type ValidationError =
  | PartDefinitionMissingName of index: int
  | PartDefinitionMissingId of index: int
  | PartDefinitionsWithRepeatedIds of PartsWithRepeatedIds
  | NotesSectionReferencesInvalidPartId of NotesSectionReferencesInvalidPartId

and PartsWithRepeatedIds = { PartId: PartId; Indexes: int list }
and NotesSectionReferencesInvalidPartId = { PartId: PartId; Index: int }

module Validated =
  type Music = List<Part>

  and Part = {
    PartId: PartId
    Name: string
    Measures: Measure list
  }

  and Measure = {
    MeasureId: MeasureId
    Parsed: ParsedMeasure
  }

  let private validatePartDefinitionSections
    (p: ParsedMusic)
    : Result<ParsedPartDefinitionSection list, ValidationError list> =
    let errorsPerPart =
      p.PartDefinitionSections
      |> List.indexed
      |> List.choose (fun (idx, pd) ->
        Some [
          if Option.isNone pd.Id then
            ValidationError.PartDefinitionMissingId idx

          if Option.isNone pd.Name then
            ValidationError.PartDefinitionMissingName idx
        ])
      |> List.concat

    let partsWithRepeteadIds =
      p.PartDefinitionSections
      |> List.indexed
      |> List.choose (fun (idx, part) -> part.Id |> Option.map (fun partId -> partId, idx))
      |> List.groupBy fst
      |> List.choose (fun (partId, idxs) ->
        if idxs.Length > 1 then
          {
            PartId = partId
            Indexes = List.map snd idxs
          }
          |> ValidationError.PartDefinitionsWithRepeatedIds
          |> Some
        else
          None)

    let errors = [ yield! errorsPerPart; yield! partsWithRepeteadIds ]

    if List.isEmpty errors then
      Ok p.PartDefinitionSections
    else
      Error errors

  let private validateNotesSections
    ({
       PartDefinitionSections = partsSections
       NotesSections = notesSections
     }: ParsedMusic)
    : Result<ParsedNotesSection list, ValidationError list> =
    let partIds = partsSections |> List.choose _.Id |> Set.ofList

    let referencesToInvalidIds =
      notesSections
      |> List.indexed
      |> List.filter (fun (_, n) -> partIds |> Set.contains n.PartId |> not)
      |> List.map (fun (idx, n) ->
        ValidationError.NotesSectionReferencesInvalidPartId { PartId = n.PartId; Index = idx })

    let errors = [ yield! referencesToInvalidIds ]

    if List.isEmpty errors then
      Ok notesSections
    else
      Error errors

  let private createFromValidParsedPart
    (partDefinitionSections: ParsedPartDefinitionSection list)
    (notesSections: ParsedNotesSection list)
    : Part list =
    let measures =
      notesSections
      |> List.groupBy _.PartId
      |> List.map (fun (partId, parsedNotesSections) ->
        partId,
        parsedNotesSections
        |> List.collect _.Measures
        |> List.mapi (fun measureId measure -> {
          MeasureId = MeasureId(measureId + 1)
          Parsed = measure
        }))
      |> Map.ofList

    // TODO: there is any strategy I can use to mitigate Option.get?
    partDefinitionSections
    |> List.map (fun partDefinition ->
      let partId = Option.get partDefinition.Id

      {
        PartId = partId
        Name = Option.get partDefinition.Name
        Measures = measures |> Map.tryFind partId |> Option.defaultValue []
      })

  let musicFromParsedMusic (p: ParsedMusic) : Result<Music, ValidationError list> =
    Ok createFromValidParsedPart
    <!> validatePartDefinitionSections p
    <!> validateNotesSections p

[<RequireQualifiedAccess>]
type Music =
  | Parsed of ParsedMusic
  | Validated of Validated.Music

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
