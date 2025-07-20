module Domain.Parser

open FParsec

open Domain.Types
open Domain.MeasureBuilder

module Types =
    [<RequireQualifiedAccess>]
    type PartDefinitionAttribute =
        | Id of int
        | Name of string
        | Clef of Clef
        | TimeSignature of TimeSignature
        | KeySignature of KeySignature

    type PartDefinition =
        { Id: PartId option
          Name: string option
          Clef: Clef option
          TimeSignature: TimeSignature option
          KeySignature: KeySignature option }

module Functions =
    open Types

    type private P<'a> = Parser<'a, unit>

    [<AutoOpen>]
    module Helpers =
        let ws = spaces
        let command s = pchar ':' >>. pstring s
        let str: P<_> = many1Satisfy (System.Char.IsWhiteSpace >> not)
        let num: P<_> = pint32

        let pclef: P<_> =
            [ "f"; "g" ] |> List.map pstring |> choice
            |>> fun v ->
                match v with
                | "g" -> Clef.G
                | _ -> failwith "TODO: clef"

        let naturalNote: P<_> =
            [ "c"; "f" ] |> List.map pstring |> choice
            |>> fun v ->
                match v with
                | "c" -> NoteName.C
                | "f" -> NoteName.F
                | _ -> failwith "TODO: natural note"

        let duration: P<_> =
            [ "4" ] |> List.map pstring |> choice
            |>> fun v ->
                match v with
                | "4" -> Duration.QuarterNote
                | _ -> failwith "TODO: duration"

    let pPartDefinitionAttribute: P<_> =
        choice
            [ attempt <| command "id" .>> ws >>. num |>> PartDefinitionAttribute.Id
              attempt <| command "name" .>> ws >>. str |>> PartDefinitionAttribute.Name
              attempt <| command "clef" .>> ws >>. pclef |>> PartDefinitionAttribute.Clef
              attempt <| command "time" .>> ws >>. num .>>. spaces1 .>>. duration
              |>> fun ((numerator, _), denominator) ->
                  PartDefinitionAttribute.TimeSignature
                      { Numerator = numerator
                        Denominator = denominator }
              attempt <| command "key" .>> ws >>. naturalNote
              |>> fun v -> v |> KeySignature |> PartDefinitionAttribute.KeySignature ]

    let pPartDefinition: P<_> =
        command "part" .>> ws >>. many (pPartDefinitionAttribute .>> ws)
        |>> (fun partDefinitionAttributes ->
            let mutable partId: PartId option = None
            let mutable name: string option = None
            let mutable clef: Clef option = None
            let mutable timeSignature: TimeSignature option = None
            let mutable keySignature: KeySignature option = None

            partDefinitionAttributes
            |> List.iter (fun attribute ->
                match attribute with
                | PartDefinitionAttribute.Id v -> partId <- (v |> PartId |> Some)
                | PartDefinitionAttribute.Name v -> name <- Some v
                | PartDefinitionAttribute.Clef v -> clef <- Some v
                | PartDefinitionAttribute.TimeSignature v -> timeSignature <- Some v
                | PartDefinitionAttribute.KeySignature v -> keySignature <- Some v)

            { Id = partId
              Name = name
              Clef = clef
              TimeSignature = timeSignature
              KeySignature = keySignature })

    // TODO: validation
    // TODO: parsing of notes
    let pMusic: P<Music> =
        pPartDefinition
        |>> (fun partDefinition ->
            let firstMeasure =
                aMeasure 1
                |> withKeySignature (Option.get partDefinition.KeySignature)
                |> withTimeSignature (Option.get partDefinition.TimeSignature)
                |> withClef (Option.get partDefinition.Clef)
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.EightNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.EightNote }
                |> withNote
                    { NoteName = NoteName.E
                      Octave = 4
                      Duration = Duration.EightNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.EightNote }

            let secondMeasure =
                aMeasure 2
                |> withKeySignature (Option.get partDefinition.KeySignature)
                |> withTimeSignature (Option.get partDefinition.TimeSignature)
                |> withClef (Option.get partDefinition.Clef)
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.HalfNote }

            Music
                [ { Id = Option.get partDefinition.Id
                    Name = Option.get partDefinition.Name
                    Measures = [ firstMeasure; secondMeasure ] } ])
