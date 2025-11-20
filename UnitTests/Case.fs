module UnitTests.Case

open Expecto

type Case<'a, 'b> = {
  Id: string
  Data: 'a
  ExpectedResult: 'b
} with

  override this.ToString() = $"{this.Id}"

type Builder<'a, 'b>(id: string) =
  let mutable data: 'a option = None
  let mutable expectedResult: 'b option = None

  member _.Data
    with get () = data
    and set v = data <- v

  member _.ExpectedResult
    with get () = expectedResult
    and set v = expectedResult <- v

  member this.WithData v =
    this.Data <- Some v
    this

  member this.WithExpectedResult v =
    this.ExpectedResult <- Some v
    this

  member this.Build() = {
    Id = id
    Data = Option.get this.Data
    ExpectedResult = Option.get this.ExpectedResult
  }

let caseId (id: int) = Builder<'a, 'b>(id.ToString())

let case (id: string) = Builder<'a, 'b> id

type TestBody<'a, 'b> = 'a -> 'b -> unit

[<System.Obsolete>]
let testTheory2<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
  testTheory name cases
  <| fun
         {
           Id = _
           Data = data
           ExpectedResult = expectedResult
         } -> testBody data expectedResult

let testTheory3<'a, 'b> (name: string) (cases: Builder<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
  testTheory name (List.map (fun (builder: Builder<'a, 'b>) -> builder.Build()) cases)
  <| fun
         {
           Id = _
           Data = data
           ExpectedResult = expectedResult
         } -> testBody data expectedResult

let ptestTheory2<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
  ptestTheory name cases
  <| fun
         {
           Id = _
           Data = data
           ExpectedResult = expectedResult
         } -> testBody data expectedResult

let ftestTheory2<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
  ftestTheory name cases
  <| fun
         {
           Id = _
           Data = data
           ExpectedResult = expectedResult
         } -> testBody data expectedResult
