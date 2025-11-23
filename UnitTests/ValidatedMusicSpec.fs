module UnitTests.ValidatedMusicSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain.Types
open Domain.MeasureBuilder

let ``invalidates wrong parsed parts`` =
  let aPart partId name clef timeSignature keySignature = {
    Id = partId
    Name = name
    Clef = clef
    TimeSignature = timeSignature
    KeySignature = keySignature
  }

  let aPartWithId partId =
    aPart
      partId
      (Some "Piano")
      (Some Clef.G)
      (Some {
        Numerator = 4
        Denominator = Duration.Quarter
      })
      (NoteName.C |> KeySignature |> Some)

  let aPartWithName name =
    aPart
      (1 |> PartId |> Some)
      name
      (Some Clef.G)
      (Some {
        Numerator = 4
        Denominator = Duration.Quarter
      })
      (NoteName.C |> KeySignature |> Some)

  testTheory3 "invalidates wrong parsed parts" [
    case("no name")
      .WithData(
        {
          PartDefinitionSections = [ "Piano" |> Some |> aPartWithName; aPartWithName None ]
          NotesSections = []
        }
      )
      .WithExpectedResult(ValidationError.PartDefinitionMissingName 1)

    case("no id")
      .WithData(
        {
          PartDefinitionSections = [ aPartWithId None; 1 |> PartId |> Some |> aPartWithId ]
          NotesSections = []
        }
      )
      .WithExpectedResult(ValidationError.PartDefinitionMissingId 0)

    case("repeated ids.1")
      .WithData(
        {
          // 1
          PartDefinitionSections = [ 1; 1; 2; 5; 10; 3; 4 ] |> List.map (PartId >> Some >> aPartWithId)
          NotesSections = []
        }
      )
      .WithExpectedResult(
        ValidationError.PartDefinitionsWithRepeatedIds {
          PartId = PartId 1
          Indexes = [ 0; 1 ]
        }
      )

    case("repeated ids.2")
      .WithData(
        {
          // 10
          PartDefinitionSections = [ 1; 2; 5; 10; 10; 3; 4; 10 ] |> List.map (PartId >> Some >> aPartWithId)
          NotesSections = []
        }
      )
      .WithExpectedResult(
        ValidationError.PartDefinitionsWithRepeatedIds {
          PartId = PartId 10
          Indexes = [ 3; 4; 7 ]
        }
      )
  ]
  <| fun parsedMusic expectedError ->
    parsedMusic
    |> Validated.musicFromParsedMusic
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
    |> Validated.musicFromParsedMusic
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
    ``invalidates wrong parsed parts``
    ``creates validated music from correct parsed music``
  ]
