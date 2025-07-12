module Domain.MusicToXml

open System.Xml.Linq

open Domain.XmlWrapper

open Domain.Types

type PartId = PartId of int

let partId2String (PartId partId) = $"P{partId}"

let indexWithPartId (xs: 'a list) : list<PartId * 'a> =
    xs |> List.indexed |> List.map (fun (idx, x) -> idx + 1 |> PartId, x)

let createPartList (names: string list) : XElement =
    names
    |> indexWithPartId
    |> List.map (fun (partId, name) ->
        element "score-part" [ attribute "id" (partId2String partId) ] [ leafElement "part-name" name ])
    |> element "part-list" []

let measureNumber2String (MeasureNumber measureNumber) = measureNumber.ToString()

let createMeasure (m: Measure) : XElement =
    element "measure" [ attribute "number" (measureNumber2String m.MeasureNumber) ] []

let createPart (measures: Measure list list) : XElement list =
    measures
    |> indexWithPartId
    |> List.map (fun (partId, measures) ->
        measures
        |> List.map createMeasure
        |> element "part" [ attribute "id" (partId2String partId) ])

let convert (m: Music) : XDocument =
    let (Music parts) = m

    [ parts |> List.map _.Name |> createPartList
      yield! parts |> List.map _.Measures |> createPart ]
    |> element "score-partwise" [ attribute "version" "4.0" ]
    |> document
