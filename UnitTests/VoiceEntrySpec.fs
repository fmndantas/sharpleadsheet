module UnitTests.VoiceEntrySpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain

let ``a voice entry made from rest don't have start tie`` =
  testTheory3 "a voice entry made from rest don't have start tie" [
    case("1.voice entry created from rest returns isTied = false when Tie modifier is added")
      .WithData(Duration.Whole |> Rest.create |> VoiceEntry.fromRest |> VoiceEntry.withTie)
      .WithExpectedResult
      false

    case("2.tied voice entry created from note")
      .WithData(
        Duration.Whole
        |> Note.create 4 NoteName.C
        |> VoiceEntry.fromNote
        |> VoiceEntry.withTie
      )
      .WithExpectedResult
      true

    case("3.not tied voice entry created from note")
      .WithData(Duration.Whole |> Note.create 4 NoteName.C |> VoiceEntry.fromNote)
      .WithExpectedResult
      false

    case("4.tied voice entry created from rhythmic note")
      .WithData(
        Duration.Whole
        |> RhythmicNote.create
        |> VoiceEntry.fromRhythmicNote
        |> VoiceEntry.withTie
      )
      .WithExpectedResult
      true
  ]
  <| fun voiceEntry expctedResult ->
    VoiceEntry.isTied voiceEntry
    |> equal "isTied return incorrect result" expctedResult

[<Tests>]
let MeasureSpec =
  testList "voice entry" [ ``a voice entry made from rest don't have start tie`` ]
