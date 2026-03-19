module UnitTests.WorkflowSpec

open System.IO

open Expecto
open Expecto.Flip.Expect

open Case

open App
open Domain

[<Literal>]
let here = __SOURCE_DIRECTORY__

let private slsFile (file: string) : string =
  let dot = Directory.GetParent(here).FullName
  Path.Join(dot, "Samples", file)

let private xmlFile file = Path.Join(here, "ExpectedXmls", file)

let ``it converts sls into xml`` =
  testTheory3 "it converts sls into xml" [
    caseId(1).WithData(slsFile "helloworld.sls").WithExpectedResult(xmlFile "helloworld.xml")
    caseId(2).WithData(slsFile "example-5.sls").WithExpectedResult(xmlFile "example-5.xml")
  ]
  <| fun slsFile expectedXmlFile ->
    let expectedContent =
      expectedXmlFile |> File.ReadAllText |> XmlWrapper.minifyPlainText

    slsFile
    |> Workflow.transformPathIntoMusicXml
    |> wantOk "result should be ok"
    |> XmlWrapper.formatXDocument
    |> equal "result is incorrect" expectedContent

[<Tests>]
let WorkflowSpec = testList "workflow" [ ``it converts sls into xml`` ]
