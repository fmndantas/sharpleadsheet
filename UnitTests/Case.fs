module UnitTests.Case

open Expecto

type Case<'a, 'b> =
    { Id: string
      Data: 'a
      ExpectedResult: 'b }

    override this.ToString() = $"{this.Id}"

type TestBody<'a, 'b> = 'a -> 'b -> unit

let testTheory2<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
    testTheory name cases
    <| fun
           { Id = _
             Data = data
             ExpectedResult = expectedResult } -> testBody data expectedResult

let ptestTheory2<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
    ptestTheory name cases
    <| fun
           { Id = _
             Data = data
             ExpectedResult = expectedResult } -> testBody data expectedResult

let ftestTheory2<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
    ftestTheory name cases
    <| fun
           { Id = _
             Data = data
             ExpectedResult = expectedResult } -> testBody data expectedResult

[<AutoOpen>]
module Builder =
    type T<'a, 'b> =
        { IdB: string
          DataB: 'a option
          ExpectedResultB: 'b option }

    let aCase<'a, 'b> (id: string) : T<'a, 'b> =
        { IdB = id
          DataB = None
          ExpectedResultB = None }

    let withData (data: 'a) (b: T<'a, 'b>) : T<'a, 'b> = { b with DataB = Some data }

    let withExpectedResult (expectedResult: 'b) (b: T<'a, 'b>) : T<'a, 'b> =
        { b with
            ExpectedResultB = Some expectedResult }

    let build (b: T<'a, 'b>) : Case<'a, 'b> =
        { Id = b.IdB
          Data = Option.get b.DataB
          ExpectedResult = Option.get b.ExpectedResultB }
