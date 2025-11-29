// For more information see https://aka.ms/fsharp-console-apps
open System

open Domain.CommonTypes

open App.Workflow

[<EntryPoint>]
let main argv =
  let source = argv[0]

  let workflowErrorToString (e: WorkflowError) =
    match e with
    | WorkflowError.Parsing s -> s
    | WorkflowError.Validation v ->
      match v with
      | ValidationError.PartDefinitionMissingId idx -> sprintf "Part %d: missing id" idx
      | ValidationError.PartDefinitionMissingName idx -> sprintf "Part %d: missing name" idx
      |> sprintf "%2s -> %s" String.Empty

  let result =
    source
    |> Path.fromString
    |> parse
    |> Result.mapError (List.map workflowErrorToString)

  match result with
  | Ok m -> printfn "Parsing went OK!"
  | Error errors ->
    printfn "Parsing generated errors :("
    errors |> List.iter (printfn "%s")

  0 // return an integer exit code
