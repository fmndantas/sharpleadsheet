module UnitTests.ValidatedMusicSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain.Types
open Domain.MeasureBuilder

let ``invalidates parts with empty name`` =
  testTheory3 "invalidates part definitions without id" [
    case("no name")
      .WithData(
        {
          PartDefinitionSections = [
            {
              Id = 1 |> PartId |> Some
              Name = None
              Clef = Some Clef.G
              TimeSignature =
                Some {
                  Numerator = 4
                  Denominator = Duration.Quarter
                }
              KeySignature = NoteName.C |> KeySignature |> Some
            }
          ]
          NotesSections = []
        }
      )
      .WithExpectedResult(ValidationError.PartDefinitionMissingName 0)

    case("no id")
      .WithData(
        {
          PartDefinitionSections = [
            {
              Id = None
              Name = Some "Piano"
              Clef = Some Clef.G
              TimeSignature =
                Some {
                  Numerator = 4
                  Denominator = Duration.Quarter
                }
              KeySignature = NoteName.C |> KeySignature |> Some
            }
          ]
          NotesSections = []
        }
      )
      .WithExpectedResult(ValidationError.PartDefinitionMissingId 0)
  ]
  <| fun parsedMusic expectedError ->
    parsedMusic
    |> ValidatedMusic.fromParsed
    |> wantError "validate should result in a error"
    |> exists "expected error was not found" ((=) expectedError)

let ``creates validated music from correct parsed music`` =
  testCase "Creates validated music from correct parsed music"
  <| fun () ->
    let parsedMusic = {
      PartDefinitionSections = [
        {
          Id = 1 |> PartId |> Some
          Name = Some "Piano"
          Clef = None
          TimeSignature = None
          KeySignature = None
        }
      ]
      NotesSections = [
        {
          PartId = PartId 1
          Measures = [ aParsedMeasure () |> withNote (Note.create4 NoteName.C Duration.Whole) ]
        }
      ]
    }

    parsedMusic
    |> ValidatedMusic.fromParsed
    |> wantOk "validation should succeed"
    |> equal "validated music is different from expected" [
      {
        PartId = PartId 1
        Name = "Piano"
        Measures = [
          {
            MeasureId = MeasureId 1
            Parsed = aParsedMeasure () |> withNote (Note.create4 NoteName.C Duration.Whole)
          }
        ]
      }
    ]

[<Tests>]
let ValidatorSpec =
  testList "ValidatorSpec" [
    ``invalidates parts with empty name``
    ``creates validated music from correct parsed music``
  ]
