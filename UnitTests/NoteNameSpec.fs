module Domain.NoteNameSpec

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain.Types

let ``calculates number of semitones until reach lower c`` =
    testTheory2
        "calculates number of semitones until reach lower c"
        [ { Id = "C"
            Data = NoteName.C
            ExpectedResult = 0 }

          { Id = "C#"
            Data = NoteName.CSharp
            ExpectedResult = 1 }

          { Id = "Db"
            Data = NoteName.DFlat
            ExpectedResult = 1 }

          { Id = "D"
            Data = NoteName.D
            ExpectedResult = 2 }

          { Id = "D#"
            Data = NoteName.DSharp
            ExpectedResult = 3 }

          { Id = "Eb"
            Data = NoteName.EFlat
            ExpectedResult = 3 }

          { Id = "E"
            Data = NoteName.E
            ExpectedResult = 4 }

          { Id = "F"
            Data = NoteName.F
            ExpectedResult = 5 }

          { Id = "F#"
            Data = NoteName.FSharp
            ExpectedResult = 6 }

          { Id = "Gb"
            Data = NoteName.GFlat
            ExpectedResult = 6 }

          { Id = "G"
            Data = NoteName.G
            ExpectedResult = 7 }

          { Id = "G#"
            Data = NoteName.GSharp
            ExpectedResult = 8 }

          { Id = "Ab"
            Data = NoteName.AFlat
            ExpectedResult = 8 }

          { Id = "A"
            Data = NoteName.A
            ExpectedResult = 9 }

          { Id = "A#"
            Data = NoteName.ASharp
            ExpectedResult = 10 }

          { Id = "Bb"
            Data = NoteName.BFlat
            ExpectedResult = 10 }

          { Id = "B"
            Data = NoteName.B
            ExpectedResult = 11 } ]
    <| fun noteName expectedResult ->
        let result = NoteName.semitonesToReachC noteName
        (expectedResult, result) ||> equal "foobar"

[<Tests>]
let Spec =
    testList "Spec" [ ``calculates number of semitones until reach lower c`` ]
