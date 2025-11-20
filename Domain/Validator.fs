module Domain.Validator

open Domain.Types

module Types =
  [<RequireQualifiedAccess>]
  type ValidationError = PartDefinitionMissingName of partIndex: int

open Types

let validate (m: Music) : Result<Music, ValidationError list> =
  match m with
  | Music.Parsed p ->
    let partsWithMissingName =
      p.PartDefinitionSections
      |> List.indexed
      |> List.choose (fun (idx, part) ->
        if Option.isNone part.Name then
          Some(ValidationError.PartDefinitionMissingName idx)
        else
          None)

    let errors = [ yield! partsWithMissingName ]

    if List.isEmpty errors then
      failwith "todo"
    else
      Error errors
  | _ -> failwith "todo"
