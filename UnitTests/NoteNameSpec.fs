module UnitTests.NoteNameSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open Domain.Types

let ``calculates number of semitones until reach lower c`` =
  let case (id: string) = Builder<NoteName, int>(id)

  testTheory3 "calculates number of semitones until reach lower c" [
    case("C").WithData(NoteName.C).WithExpectedResult(0)
    case("C#").WithData(NoteName.CSharp).WithExpectedResult(1)
    case("Db").WithData(NoteName.DFlat).WithExpectedResult(1)
    case("D").WithData(NoteName.D).WithExpectedResult(2)
    case("D#").WithData(NoteName.DSharp).WithExpectedResult(3)
    case("Eb").WithData(NoteName.EFlat).WithExpectedResult(3)
    case("E").WithData(NoteName.E).WithExpectedResult(4)
    case("F").WithData(NoteName.F).WithExpectedResult(5)
    case("F#").WithData(NoteName.FSharp).WithExpectedResult(6)
    case("Gb").WithData(NoteName.GFlat).WithExpectedResult(6)
    case("G").WithData(NoteName.G).WithExpectedResult(7)
    case("G#").WithData(NoteName.GSharp).WithExpectedResult(8)
    case("Ab").WithData(NoteName.AFlat).WithExpectedResult(8)
    case("A").WithData(NoteName.A).WithExpectedResult(9)
    case("A#").WithData(NoteName.ASharp).WithExpectedResult(10)
    case("Bb").WithData(NoteName.BFlat).WithExpectedResult(10)
    case("B").WithData(NoteName.B).WithExpectedResult(11)
  ]
  <| fun noteName expectedResult ->
    let result = NoteName.semitonesToReachC noteName
    (expectedResult, result) ||> equal "Result is incorrect"

[<Tests>]
let Spec =
  testList "Spec" [ ``calculates number of semitones until reach lower c`` ]
