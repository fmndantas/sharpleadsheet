module UnitTests.LearnDeepEqual

open Expecto
open Expecto.Flip.Expect

open DeepEqual.Syntax

open Domain
open Domain.CommonTypes
open Domain.ParsedTypes

open Domain.ParsedMeasureBuilder

let diffWithExpecto = testCase "diff with expecto" <| fun () -> 1 |> equal "equal" 2

let diffWithDeepEqual =
  testCase "diff with deep equal"
  <| fun () ->
    let actual = {| Value = 1 |}
    let expected = {| Value = 2 |}
    actual.ShouldDeepEqual expected

let diffInNotes =
  testCase "diff in notes"
  <| fun () ->
    let actual = Note.create4 NoteName.C Duration.Whole
    let expected = Note.create4 NoteName.D Duration.Whole
    actual.ShouldDeepEqual expected

let diffInParsedNotesSection =
  testCase "diff in parsed notes section"
  <| fun () ->
    let actual = {
      PartId = PartId 1
      Measures =
        aParsedMeasure ()
        |> withNote (Note.create4 NoteName.C Duration.Whole)
        |> List.singleton
    }

    let expected = {
      PartId = PartId 1
      Measures =
        aParsedMeasure ()
        |> withNote (Note.create4 NoteName.C Duration.Quarter)
        |> List.singleton
    }

    actual.ShouldDeepEqual expected

[<Tests>]
let tests =
  testList "learn deep equal" [ diffWithExpecto; diffWithDeepEqual; diffInParsedNotesSection; diffInNotes ]
