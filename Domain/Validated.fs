module Domain.Validated

open Domain.ParsedTypes
open Domain.CommonTypes

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

let private getMeasuresPerPartId (notesSections: ParsedNotesSection list) : (PartId * Measure list) list =
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

let private validateMeasure (partId: PartId, measure: Measure) : Result<Measure, ValidationError list> =
  let {
        Numerator = numerator
        Denominator = denominator
      } =
    measure.Parsed.TimeSignature

  let noteOrRestToDuration =
    function
    | NoteOrRest.Note n -> Note.getDuration n
    | NoteOrRest.Rest(Rest d) -> d

  if
    (List.replicate numerator denominator, List.map noteOrRestToDuration measure.Parsed.NotesOrRests)
    ||> Duration.getEquivalenceBetweenLists
  then
    Ok measure
  else
    Error [ ValidationError.MeasureWithInconsistentDurations(measure.MeasureId, partId) ]

let private validateNotesSections
  ({
     PartDefinitionSections = partsSections
     NotesSections = notesSections
   }: ParsedMusic)
  : Result<ParsedNotesSection list, ValidationError list> =
  let partIds = partsSections |> List.choose _.Id |> Set.ofList

  let referencesToInvalidIds: ValidationError list =
    notesSections
    |> List.indexed
    |> List.filter (fun (_, n) -> partIds |> Set.contains n.PartId |> not)
    |> List.map (fun (idx, n) -> ValidationError.NotesSectionReferencesInvalidPartId { PartId = n.PartId; Index = idx })

  let errorsPerMeasure: ValidationError list =
    notesSections
    |> getMeasuresPerPartId
    |> List.collect (fun (partId, measures) -> List.map (fun measure -> partId, measure) measures)
    |> List.map validateMeasure
    |> Result.traverse
    |> function
      | Ok _ -> []
      | Error errors -> errors

  let errors = [ yield! referencesToInvalidIds; yield! errorsPerMeasure ]

  if List.isEmpty errors then
    Ok notesSections
  else
    Error errors

let private createFromValidParsedPart
  (partDefinitionSections: ParsedPartDefinitionSection list)
  (notesSections: ParsedNotesSection list)
  : Part list =
  let measures = notesSections |> getMeasuresPerPartId |> Map.ofList

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
