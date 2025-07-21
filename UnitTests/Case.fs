module UnitTests.Case

open Expecto

type Case<'a, 'b> =
    { Id: string
      Data: 'a
      ExpectedResult: 'b }

    override this.ToString() = $"{this.Id}"

type TestBody<'a, 'b> = 'a -> 'b -> unit

let tt<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
    testTheory name cases
    <| fun
           { Id = _
             Data = data
             ExpectedResult = expectedResult } -> testBody data expectedResult

let ptt<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
    ptestTheory name cases
    <| fun
           { Id = _
             Data = data
             ExpectedResult = expectedResult } -> testBody data expectedResult

let ftt<'a, 'b> (name: string) (cases: Case<'a, 'b> list) (testBody: TestBody<'a, 'b>) =
    ftestTheory name cases
    <| fun
           { Id = _
             Data = data
             ExpectedResult = expectedResult } -> testBody data expectedResult
