module UnitTests.KeySignature

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.Types

let ``it should calculate fifths`` =
    testTheory2
        "it should calculate fifths"
        [ { Id = "C"
            Data = KeySignature NoteName.C
            ExpectedResult = Fifth.Zero }

          { Id = "F"
            Data = KeySignature NoteName.F
            ExpectedResult = Fifth.Flat 1 }

          { Id = "G"
            Data = KeySignature NoteName.G
            ExpectedResult = Fifth.Sharp 1 }

          { Id = "B"
            Data = KeySignature NoteName.B
            ExpectedResult = Fifth.Sharp 5 }

          { Id = "C#"
            Data = KeySignature NoteName.CSharp
            ExpectedResult = Fifth.Flat 5 }

          { Id = "Db"
            Data = KeySignature NoteName.DFlat
            ExpectedResult = Fifth.Flat 5 }

          { Id = "E"
            Data = KeySignature NoteName.E
            ExpectedResult = Fifth.Sharp 4 }

          { Id = "Ab"
            Data = KeySignature NoteName.AFlat
            ExpectedResult = Fifth.Flat 4 } ]
    <| fun keySignature expectedFifth ->
        let result = KeySignature.fifths keySignature
        (expectedFifth, result) ||> equal "Calculated fifths is incorrect"

let ``returns cicle of fifths`` =
    testCase "returns cicle of fifhts"
    <| fun () ->
        NoteName.cicleOfFifths
        |> equal
            "Cicle of fifths is incorrect"
            [ NoteName.C
              NoteName.G
              NoteName.D
              NoteName.A
              NoteName.E
              NoteName.B
              NoteName.FSharp
              NoteName.CSharp
              NoteName.GSharp
              NoteName.DSharp
              NoteName.ASharp
              NoteName.F ]

let ``returns circle of fourths`` =
    testCase "returns circle of fourths"
    <| fun () ->
        NoteName.cicleOfFourths
        |> equal
            "Circle of fourths is incorrect"
            [ NoteName.C
              NoteName.F
              NoteName.BFlat
              NoteName.EFlat
              NoteName.AFlat
              NoteName.DFlat
              NoteName.GFlat
              NoteName.B
              NoteName.E
              NoteName.A
              NoteName.D
              NoteName.G ]

[<Tests>]
let MusicToXmlSpec =
    testList
        "MusicToXmlSpec"
        [ ``it should calculate fifths``
          ``returns cicle of fifths``
          ``returns circle of fourths`` ]
