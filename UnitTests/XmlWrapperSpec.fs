module UnitTests.XmlWrapperSpec

open Expecto
open Expecto.Flip.Expect

open System.Xml.Linq

open Domain.XmlWrapper

let ``it should create xelement with attributes`` =
  testCase "it should create xelement with attributes"
  <| fun () ->
    let result =
      elementWithAttributes "name" [ attribute "key" "value" ] [ leafElement "leafName" "leafContent" ]

    let expectedResult =
      XElement(XName.Get "name", XAttribute(XName.Get "key", "value"), XElement(XName.Get "leafName", "leafContent"))

    (expectedResult.ToString(), result.ToString()) ||> equal "XElement is incorrect"

[<Tests>]
let XmlWrapperSpec =
  testList "XmlWrapperSpec" [ ``it should create xelement with attributes`` ]
