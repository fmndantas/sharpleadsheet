module UnitTest.FParsecSpec

open Expecto

open Domain

let main = testCase "main" <| fun () -> FParsec.main ()

[<FTests>]
let FParsecSpec = testList "FParsecSpec" [ main ]
