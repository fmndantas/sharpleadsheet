module UnitTests.NoteOrRestSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain

let ``a noteOrRest made from rest cannot have start tie`` =
  testTheory3 "a noteOrRest made from rest cannot have start tie" [
    case("1.noteOrRest created from rest returns isTied = false when Tie modifier is added")
      .WithData(Duration.Whole |> Rest.create |> NoteOrRest.fromRest |> NoteOrRest.withTie)
      .WithExpectedResult
      false
    case("2.tied noteOrRest created from note")
      .WithData(
        Duration.Whole
        |> Note.create 4 NoteName.C
        |> NoteOrRest.fromNote
        |> NoteOrRest.withTie
      )
      .WithExpectedResult
      true

    case("3.not tied noteOrRest created from note")
      .WithData(Duration.Whole |> Note.create 4 NoteName.C |> NoteOrRest.fromNote)
      .WithExpectedResult
      false
  ]
  <| fun noteOrRest expctedResult ->
    NoteOrRest.isTied noteOrRest
    |> equal "isTied return incorrect result" expctedResult

[<Tests>]
let MeasureSpec =
  testList "note or rest" [ ``a noteOrRest made from rest cannot have start tie`` ]
