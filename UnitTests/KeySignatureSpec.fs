module UnitTests.KeySignature

open Expecto
open Expecto.Flip.Expect

open UnitTests.Case

open Domain
open Domain.CommonTypes

let ``it should calculate fifths`` =
  testTheory3 "it should calculate fifths" [
    case("C").WithData(KeySignature NoteName.C).WithExpectedResult Fifth.Zero
    case("F").WithData(KeySignature NoteName.F).WithExpectedResult(Fifth.Flat 1)
    case("G").WithData(KeySignature NoteName.G).WithExpectedResult(Fifth.Sharp 1)
    case("B").WithData(KeySignature NoteName.B).WithExpectedResult(Fifth.Sharp 5)
    case("C#").WithData(KeySignature NoteName.CSharp).WithExpectedResult(Fifth.Flat 5)
    case("Db").WithData(KeySignature NoteName.DFlat).WithExpectedResult(Fifth.Flat 5)
    case("E").WithData(KeySignature NoteName.E).WithExpectedResult(Fifth.Sharp 4)
    case("Ab").WithData(KeySignature NoteName.AFlat).WithExpectedResult(Fifth.Flat 4)
  ]
  <| fun keySignature expectedFifth ->
    let result = KeySignature.fifths keySignature
    (expectedFifth, result) ||> equal "Calculated fifths is incorrect"

let ``returns cicle of fifths`` =
  testCase "returns cicle of fifhts"
  <| fun () ->
    NoteName.cicleOfFifths
    |> equal "Cicle of fifths is incorrect" [
      NoteName.C
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
      NoteName.F
    ]

let ``returns circle of fourths`` =
  testCase "returns circle of fourths"
  <| fun () ->
    NoteName.cicleOfFourths
    |> equal "Circle of fourths is incorrect" [
      NoteName.C
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
      NoteName.G
    ]

[<Tests>]
let MusicToXmlSpec =
  testList "MusicToXmlSpec" [
    ``it should calculate fifths``
    ``returns cicle of fifths``
    ``returns circle of fourths``
  ]
