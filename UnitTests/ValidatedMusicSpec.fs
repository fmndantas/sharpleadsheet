module UnitTests.ValidatedMusicSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain.Types

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
    |> ValidatedMusic.fromParsed
    |> wantError "validate should result in a error"
    |> exists "expected error was not found" ((=) expectedError)

[<Tests>]
let ValidatorSpec =
  testList "ValidatorSpec" [ ``invalidates parts with empty name`` ]
