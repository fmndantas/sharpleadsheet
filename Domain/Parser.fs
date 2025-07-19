module Domain.Parser

open FParsec

open Domain.Types
open Domain.MeasureBuilder

module Types =
    [<RequireQualifiedAccess>]
    type PartDefinitionAttribute =
        | Id of int
        | Name of string
        | Clef of string
        | TimeSignature of int * int
        | KeySignature of string

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
        let key s = pstring s .>> ws
        let str: P<_> = many1Satisfy (System.Char.IsWhiteSpace >> not) .>> ws
        let num: P<_> = pint32 .>> ws
        let clef = many1Satisfy (fun c -> c = 'f' || c = 'g') .>> ws

    let pPartDefinitionAttribute: P<_> =
        choice
            [ key ":id" >>. num |>> PartDefinitionAttribute.Id
              key ":name" >>. str |>> PartDefinitionAttribute.Name
              key ":clef" >>. clef |>> PartDefinitionAttribute.Clef
              key ":time" >>. num .>>. num
              |>> fun (a, b) -> PartDefinitionAttribute.TimeSignature(a, b)
              key ":key" >>. str |>> PartDefinitionAttribute.KeySignature ]

    let pPartDefinition: P<_> = key ":part" >>. many pPartDefinitionAttribute

    // TODO: validation
    let parsePartDefinition (content: string) : Result<PartDefinition, string> =
        match run pPartDefinition content with
        | Success(partDefinition, _, _) ->
            let mutable partId: PartId option = None
            let mutable name: string option = None
            let mutable clef: string option = None
            let mutable timeSignature: option<int * int> = None

            partDefinition
            |> List.iter (fun d ->
                match d with
                | PartDefinitionAttribute.Id v -> partId <- (v |> PartId |> Some)
                | PartDefinitionAttribute.Name v -> name <- Some v
                | PartDefinitionAttribute.Clef v -> clef <- Some v
                | PartDefinitionAttribute.TimeSignature(a, b) -> timeSignature <- Some(a, b)
                | _ -> ())

            Result.Ok
                { Id = partId
                  Name = name
                  // TODO: treat other clefs
                  Clef = Option.bind (fun c -> if c = "g" then Some Clef.G else None) clef
                  TimeSignature =
                    Option.bind
                        (fun (a, _) ->
                            Some
                                { Numerator = a
                                  // FIX: denominator
                                  Denominator = Duration.WholeNote })
                        timeSignature
                  KeySignature = None }
        | Failure(error, _, _) -> Result.Error error

    // TODO: validation
    let parse (content: string) : Result<Music, string> =
        [ { Name = "Piano"
            Id = PartId 1
            Measures =
              [ aMeasure 1
                |> withCNaturalKeySignature
                |> withTimeSignature
                    { Numerator = 2
                      Denominator = Duration.HalfNote }
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

                aMeasure 2
                |> withCNaturalKeySignature
                |> withTimeSignature
                    { Numerator = 2
                      Denominator = Duration.HalfNote } ] } ]
        |> Music
        |> Result.Ok
