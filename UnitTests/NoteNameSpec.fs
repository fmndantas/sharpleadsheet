module UnitTests.NoteNameSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open Domain.Types

let ``calculates number of semitones until reach lower c`` =
    testTheory2
        "calculates number of semitones until reach lower c"
        [ aCase "C" |> withData NoteName.C |> withExpectedResult 0 |> build
          aCase "C#" |> withData NoteName.CSharp |> withExpectedResult 1 |> build
          aCase "Db" |> withData NoteName.DFlat |> withExpectedResult 1 |> build
          aCase "D" |> withData NoteName.D |> withExpectedResult 2 |> build
          aCase "D#" |> withData NoteName.DSharp |> withExpectedResult 3 |> build
          aCase "Eb" |> withData NoteName.EFlat |> withExpectedResult 3 |> build
          aCase "E" |> withData NoteName.E |> withExpectedResult 4 |> build
          aCase "F" |> withData NoteName.F |> withExpectedResult 5 |> build
          aCase "F#" |> withData NoteName.FSharp |> withExpectedResult 6 |> build
          aCase "Gb" |> withData NoteName.GFlat |> withExpectedResult 6 |> build
          aCase "G" |> withData NoteName.G |> withExpectedResult 7 |> build
          aCase "G#" |> withData NoteName.GSharp |> withExpectedResult 8 |> build
          aCase "Ab" |> withData NoteName.AFlat |> withExpectedResult 8 |> build
          aCase "A" |> withData NoteName.A |> withExpectedResult 9 |> build
          aCase "A#" |> withData NoteName.ASharp |> withExpectedResult 10 |> build
          aCase "Bb" |> withData NoteName.BFlat |> withExpectedResult 10 |> build
          aCase "B" |> withData NoteName.B |> withExpectedResult 11 |> build ]
    <| fun noteName expectedResult ->
        let result = NoteName.semitonesToReachC noteName
        (expectedResult, result) ||> equal "Result is incorrect"

[<Tests>]
let Spec =
    testList "Spec" [ ``calculates number of semitones until reach lower c`` ]
