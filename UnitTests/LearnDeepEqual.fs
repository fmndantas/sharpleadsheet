module UnitTests.LearnDeepEqual

open System

open Expecto
open Expecto.Flip.Expect

open DeepEqual.Syntax

open Domain
open Domain.CommonTypes
open Domain.ParsedTypes

open Domain.ParsedMeasureBuilder

[<AutoOpen>]
module private ExpectoDeepEqual =
  let diffPrinter (actual: obj) (expected: obj) : string =
    let mutable message = ""

    try
      actual.ShouldDeepEqual expected
    with ex ->
      message <- ex.Message

    let fragments =
      let fs = message.Split(Environment.NewLine).[1..]
      let h = Environment.NewLine + Array.head fs
      [| h; yield! Array.tail fs |]

    String.Join(Environment.NewLine, fragments)

  let deepEqual (actual: obj) (expected: obj) =
    Expect.equalWithDiffPrinter diffPrinter actual expected "DeepEqual comparison failed"

let case1 = testCase "case 1" <| fun () -> 1 |> equal "equal" 2

let case2 =
  testCase "case 2"
  <| fun () ->
    let actual = {| Value = 1 |}
    let expected = {| Value = 2 |}
    actual |> deepEqual expected

let case3 =
  testCase "case 3"
  <| fun () ->
    let actual = Note.create4 NoteName.C Duration.Whole
    let expected = Note.create4 NoteName.D Duration.Whole
    actual |> deepEqual expected

// NOTE: It's better to use this way (ShouldDeepEqual)
// `deepEqual` prints less useful diff for complex types because it includes Expecto's information "<this> vs. <that>".
let case4 =
  testCase "case 4"
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

[<PTests>]
let tests = testList "learn deep equal" [ case1; case2; case3; case4 ]
