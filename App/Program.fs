// For more information see https://aka.ms/fsharp-console-apps
open System

open Domain.CommonTypes

open App.Workflow

[<EntryPoint>]
let main argv =
  let source = argv[0]
  let outputFile = argv[1]

  let partIdToString =
    function
    | PartId v -> v

  let measureIdToString =
    function
    | MeasureId v -> v

  let workflowErrorToString (e: WorkflowError) =
    match e with
    | WorkflowError.Parsing s -> s
    | WorkflowError.Validation v ->
      (match v with
       | ValidationError.PartDefinitionMissingId index -> sprintf "Part %d: missing id" index
       | ValidationError.PartDefinitionMissingName index -> sprintf "Part %d: missing name" index
       | ValidationError.PartDefinitionsWithRepeatedIds { PartId = partId; Indexes = indexes } ->
         sprintf
           "Duplicate part definition with ID %d found at the following positions: %A"
           (partIdToString partId)
           indexes
       | ValidationError.NotesSectionReferencesInvalidPartId { PartId = partId; Index = index } ->
         sprintf "Notes section %d: reference to invalid part id \"%d\"" index (partIdToString partId)
       | ValidationError.MeasureWithInconsistentDurations(measureId, partId) ->
         sprintf
           "Measure %d in part %d has inconsistent durations"
           (measureIdToString measureId)
           (partIdToString partId))
      |> sprintf "%2s -> %s" String.Empty

  let result =
    source
    |> Path.fromString
    |> parse
    |> Result.map Domain.MusicToXml.convert
    |> Result.mapError (List.map workflowErrorToString)

  match result with
  | Ok xml ->
    printfn "Parsing went OK! Saving output .xml in \"%s\"" outputFile
    System.IO.File.WriteAllText(outputFile, xml.ToString())
  | Error errors ->
    printfn "Parsing generated errors :("
    errors |> List.iter (printfn "%s")

  0 // return an integer exit code
