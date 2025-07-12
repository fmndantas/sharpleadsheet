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

// TEST: calculateFifths
let calculateFifths (k: KeySignature) : string = "0"

// TEST: calculateBeatType
let calculateBeatType (t: TimeSignature) : string = "4"

let createMeasureAttributes (es: MeasureEvent list) : XElement =
    es
    |> List.map (fun e ->
        match e with
        | MeasureEvent.DefineKeySignature k -> element "key" [] [ leafElement "fifths" (calculateFifths k) ]
        | MeasureEvent.DefineTimeSignature t ->
            element
                "time"
                []
                [ leafElement "beats" (t.Numerator.ToString())
                  leafElement "beat-type" (calculateBeatType t) ])
    |> element "attributes" []

let createMeasure (previous: Measure option, current: Measure) : XElement =
    [ (previous, current) ||> Measure.generateEvents |> createMeasureAttributes ]
    |> element "measure" [ attribute "number" (measureNumber2String current.MeasureNumber) ]

let createPart (measures: Measure list list) : XElement list =
    measures
    |> indexWithPartId
    |> List.map (fun (partId, measures) ->
        let pairsOfMeasures =
            if List.isEmpty measures then
                []
            else
                List.append
                    [ None, List.head measures ]
                    (measures |> List.pairwise |> List.map (fun (a, b) -> Some a, b))

        pairsOfMeasures
        |> List.map createMeasure
        |> element "part" [ attribute "id" (partId2String partId) ])

// TODO: add validation
let convert (m: Music) : XDocument =
    let (Music parts) = m

    [ parts |> List.map _.Name |> createPartList
      yield! parts |> List.map _.Measures |> createPart ]
    |> element "score-partwise" [ attribute "version" "4.0" ]
    |> document
