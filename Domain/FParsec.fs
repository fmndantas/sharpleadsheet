module Domain.FParsec

open FParsec

type Parser<'t> = Parser<'t, unit>

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Sucess: %A" result
    | Failure(errorMsg, _, _) -> printfn "Sucess: %s" errorMsg

let case1 () =
    test pfloat "1.25"
    test pfloat "1.25e3"
    test pfloat "1.25E3"
    test pfloat "fernando"
    test pfloat "fernando 1.25"

let str s = pstring s

let floatBetweenBrackets: Parser<float> = str "[" >>. pfloat .>> str "]"

let case2 () =
    test floatBetweenBrackets "[1.25]"
    test floatBetweenBrackets "[ 1.25]"
    test floatBetweenBrackets "[10]"

let case3 () =
    test (many floatBetweenBrackets) ""
    test (many floatBetweenBrackets) "()()()"
    test (many floatBetweenBrackets) "[10][9][123]"
    test (many floatBetweenBrackets) "[1][fernando]"
    test (many floatBetweenBrackets) "[1e9][2e100]"
    test (many floatBetweenBrackets) "[]"
    // many1 ~~> at least one element
    test (many1 floatBetweenBrackets) ""
    test (many1 floatBetweenBrackets) "[1]"

let floatList: Parser<float list> = str "[" >>. sepBy pfloat (str ",") .>> str "]"

let case4 () =
    test floatList "[9.0]"
    test floatList "[1.0,2.0,9.0]"
    test floatList "[1.0, 2.0,9.0]"
    test floatList "[1,2.0,9]"

let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws: Parser<float> = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

let case5 () = 
    test numberList "[1, 2, 3]"
    test numberList "[1,                        2,3.0]"
    test numberList @"[1, 
                             2,
                             3.0
                            ]"

let main () =
    // case1 ()
    // case2 ()
    // case3 ()
    // case4 ()
    // case5 ()
    ()
