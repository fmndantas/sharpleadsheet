module UnitTests.ValidatedMusicSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain.Types
open Domain.MeasureBuilder

[<AutoOpen>]
module private ArrangeUtils =
  let aPart partId name clef timeSignature keySignature = {
    Id = partId
    Name = name
    Clef = clef
    TimeSignature = timeSignature
    KeySignature = keySignature
  }

  let partWithId partId =
    aPart
      partId
      (Some "Piano")
      (Some Clef.G)
      (Some {
        Numerator = 4
        Denominator = Duration.Quarter
      })
      (NoteName.C |> KeySignature |> Some)

  let partWithName name =
    aPart
      (1 |> PartId |> Some)
      name
      (Some Clef.G)
      (Some {
        Numerator = 4
        Denominator = Duration.Quarter
      })
      (NoteName.C |> KeySignature |> Some)

  let notesSectionWithId partId = { PartId = partId; Measures = [] }

let ``invalidates wrong parsed parts`` =
  testTheory3 "invalidates wrong parsed parts" [
    case("no name")
      .WithData([ "Piano" |> Some |> partWithName; partWithName None ])
      .WithExpectedResult(ValidationError.PartDefinitionMissingName 1)

    case("no id")
      .WithData([ partWithId None; 1 |> PartId |> Some |> partWithId ])
      .WithExpectedResult(ValidationError.PartDefinitionMissingId 0)

    case("repeated ids.1")
      .WithData([ 1; 1; 2; 5; 10; 3; 4 ] |> List.map (PartId >> Some >> partWithId))
      .WithExpectedResult(
        ValidationError.PartDefinitionsWithRepeatedIds {
          PartId = PartId 1
          Indexes = [ 0; 1 ]
        }
      )

    case("repeated ids.2")
      .WithData([ 1; 2; 5; 10; 10; 3; 4; 10 ] |> List.map (PartId >> Some >> partWithId))
      .WithExpectedResult(
        ValidationError.PartDefinitionsWithRepeatedIds {
          PartId = PartId 10
          Indexes = [ 3; 4; 7 ]
        }
      )
  ]
  <| fun partDefinitionSections expectedError ->
    {
      PartDefinitionSections = partDefinitionSections
      NotesSections = []
    }
    |> Validated.musicFromParsedMusic
    |> wantError "validate should result in a error"
    |> exists "expected error was not found" ((=) expectedError)

let ``invalidates wrong parsed notes sections`` =
  testTheory3 "invalidates wrong parsed notes sections" [
    case("mention of invalid part id.1")
      .WithData([ 1 |> PartId |> Some |> partWithId ], [ 2 |> PartId |> notesSectionWithId ])
      .WithExpectedResult(ValidationError.NotesSectionReferencesInvalidPartId { PartId = PartId 2; Index = 0 })

    case("mention of invalid part id.2")
      .WithData(
        [ 10 |> PartId |> Some |> partWithId ],
        [
          10 |> PartId |> notesSectionWithId
          10 |> PartId |> notesSectionWithId
          234 |> PartId |> notesSectionWithId
        ]
      )
      .WithExpectedResult(ValidationError.NotesSectionReferencesInvalidPartId { PartId = PartId 234; Index = 2 })

    case("mention of invalid part id.3")
      .WithData(
        [ 7 |> PartId |> Some |> partWithId; 6 |> PartId |> Some |> partWithId ],
        [
          6 |> PartId |> notesSectionWithId
          8 |> PartId |> notesSectionWithId
          7 |> PartId |> notesSectionWithId
        ]
      )
      .WithExpectedResult(ValidationError.NotesSectionReferencesInvalidPartId { PartId = PartId 8; Index = 1 })
  ]
  <| fun (parts, notes) expectedError ->
    {
      PartDefinitionSections = parts
      NotesSections = notes
    }
    |> Validated.musicFromParsedMusic
    |> wantError "validate should result in a error"
    |> exists "expected error was not found" ((=) expectedError)

let ``creates validated music from correct parsed music`` =
  testCase "creates validated music from correct parsed music"
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
  testList "validator" [
    ``invalidates wrong parsed parts``
    ``invalidates wrong parsed notes sections``
    ``creates validated music from correct parsed music``
  ]
