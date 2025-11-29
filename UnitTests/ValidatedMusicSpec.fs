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
  let c4WithDuration duration = Note.create4 NoteName.C duration

  type ExpectedValidationResult =
    | NoError
    | ContainsError of ValidationError

  let testValidation expectedResult result =
    match expectedResult with
    | NoError -> isOk "result is supposed to be ok" result
    | ContainsError error ->
      result
      |> wantError "result is supposed to be an error"
      |> exists "expected error not found" ((=) error)

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
          Measures = [ aParsedMeasure () |> withNote (c4WithDuration Duration.Whole) ]
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
            Parsed = aParsedMeasure () |> withNote (c4WithDuration Duration.Whole)
          }
        ]
      }
    ]

let ``invalidates wrong parsed measures`` =
  let measure =
    aParsedMeasure () |> withCommonTimeSignature |> withCNaturalKeySignature

  let correctMeasure = measure |> withNote (Note.create4 NoteName.C Duration.Whole)

  let partId = PartId 1

  [
    testTheory3 "sum of durations" [
      case("1.ok").WithData([ correctMeasure ]).WithExpectedResult NoError

      case("2.error")
        .WithData([ correctMeasure; measure |> withNote (c4WithDuration Duration.Quarter) ])
        .WithExpectedResult(
          (MeasureId 2, partId)
          |> ValidationError.MeasureWithInconsistentDurations
          |> ContainsError
        )

      case("3.ok")
        .WithData(
          [
            measure
            |> withNotes (
              [
                c4WithDuration Duration.QuarterDotted
                c4WithDuration Duration.QuarterDotted
                c4WithDuration Duration.Quarter
              ]
            )
          ]
        )
        .WithExpectedResult(NoError)

      case("4.ok")
        .WithData(
          [
            measure
            |> withNotes (
              [
                c4WithDuration Duration.Sixteenth
                c4WithDuration Duration.Sixteenth
                c4WithDuration Duration.Eighth
                c4WithDuration Duration.Eighth
                c4WithDuration Duration.Eighth
                c4WithDuration Duration.QuarterDotted
                c4WithDuration Duration.Eighth
              ]
            )
          ]
        )
        .WithExpectedResult(NoError)

      case("5.error")
        .WithData(
          [
            correctMeasure
            correctMeasure
            measure |> withNote (c4WithDuration Duration.WholeDotted)
          ]
        )
        .WithExpectedResult(
          (MeasureId 3, partId)
          |> ValidationError.MeasureWithInconsistentDurations
          |> ContainsError
        )
    ]
    <| fun parsedMeasures expectedResult ->
      {
        PartDefinitionSections = [ partId |> Some |> partWithId ]
        NotesSections = [
          {
            PartId = partId
            Measures = parsedMeasures
          }
        ]
      }
      |> Validated.musicFromParsedMusic
      |> testValidation expectedResult
  ]
  |> testList "invalidates wrong parsed measures"

[<Tests>]
let ValidatorSpec =
  testList "validated music" [
    ``invalidates wrong parsed parts``
    ``invalidates wrong parsed notes sections``
    ``creates validated music from correct parsed music``
    ``invalidates wrong parsed measures``
  ]
