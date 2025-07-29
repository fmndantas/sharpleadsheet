module UnitTests.NoteNameSpec

open Expecto
open Expecto.Flip.Expect

open Case

open Domain
open Domain.Types

let ``calculates number of semitones until reach lower c`` =
    let case (id: string) = Builder<NoteName, int>(id)

    testTheory2
        "calculates number of semitones until reach lower c"
        [ case("C").WithData(NoteName.C).WithExpectedResult(0).Build()
          case("C").WithData(NoteName.C).WithExpectedResult(0).Build()
          case("C#").WithData(NoteName.CSharp).WithExpectedResult(1).Build()
          case("Db").WithData(NoteName.DFlat).WithExpectedResult(1).Build()
          case("D").WithData(NoteName.D).WithExpectedResult(2).Build()
          case("D#").WithData(NoteName.DSharp).WithExpectedResult(3).Build()
          case("Eb").WithData(NoteName.EFlat).WithExpectedResult(3).Build()
          case("E").WithData(NoteName.E).WithExpectedResult(4).Build()
          case("F").WithData(NoteName.F).WithExpectedResult(5).Build()
          case("F#").WithData(NoteName.FSharp).WithExpectedResult(6).Build()
          case("Gb").WithData(NoteName.GFlat).WithExpectedResult(6).Build()
          case("G").WithData(NoteName.G).WithExpectedResult(7).Build()
          case("G#").WithData(NoteName.GSharp).WithExpectedResult(8).Build()
          case("Ab").WithData(NoteName.AFlat).WithExpectedResult(8).Build()
          case("A").WithData(NoteName.A).WithExpectedResult(9).Build()
          case("A#").WithData(NoteName.ASharp).WithExpectedResult(10).Build()
          case("Bb").WithData(NoteName.BFlat).WithExpectedResult(10).Build()
          case("B").WithData(NoteName.B).WithExpectedResult(11).Build() ]
    <| fun noteName expectedResult ->
        let result = NoteName.semitonesToReachC noteName
        (expectedResult, result) ||> equal "Result is incorrect"

[<Tests>]
let Spec =
    testList "Spec" [ ``calculates number of semitones until reach lower c`` ]
