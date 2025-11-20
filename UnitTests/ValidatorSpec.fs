module UnitTests.ValidatorSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open Domain.Types
open Domain.Validator.Types

let ``invalidates parts with empty name`` =
  testTheory3 "invalidates part definitions without id" [
    case("invalid")
      .WithData(
        {
          PartDefinitionSections = [
            {
              Id = None
              Name = None
              Clef = None
              TimeSignature = None
              KeySignature = None
            }
          ]
          NotesSections = []
        }
      )
      .WithExpectedResult(ValidationError.PartDefinitionMissingName 0)
  ]
  <| fun parsedMusic expectedError ->
    parsedMusic
    |> Music.Parsed
    |> Validator.validate
    |> wantError "validate should result in a error"
    |> exists "expected error was not found" ((=) expectedError)

[<Tests>]
let ValidatorSpec =
  testList "ValidatorSpec" [ ``invalidates parts with empty name`` ]
