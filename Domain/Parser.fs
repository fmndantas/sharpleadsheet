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

    type ParsingState =
        { PreviousTimeSignature: TimeSignature
          PreviousKeySignature: KeySignature
          LastNote: Note option }

module Functions =
    open Types

    type private P<'a> = Parser<'a, ParsingState>

    [<AutoOpen>]
    module private Helpers =
        let ws = spaces
        let command s = pchar ':' >>. pstring s
        let str: P<_> = many1Satisfy (System.Char.IsWhiteSpace >> not)
        let num: P<_> = pint32

    let pclef: P<_> =
        [ "f"; "g" ] |> List.map pstring |> choice
        |>> fun v ->
            match v with
            | "g" -> Clef.G
            | _ -> failwith "Unknown clef: \"{v}\""

    let pNoteName: P<_> =
        [ "c"; "d"; "e"; "f"; "g"; "a"; "b" ] |> List.map pstring |> choice
        |>> fun v ->
            match v with
            | "c" -> NoteName.C
            | "d" -> NoteName.D
            | "e" -> NoteName.E
            | "f" -> NoteName.F
            | "g" -> NoteName.G
            | "a" -> NoteName.A
            | "b" -> NoteName.B
            | _ -> failwith $"Unknown note name: \"{v}\""

    let pDuration: P<_> =
        [ "16"; "8"; "4"; "2"; "1" ] |> List.map pstring |> choice
        |>> fun v ->
            match v with
            | "1" -> Duration.WholeNote
            | "2" -> Duration.HalfNote
            | "4" -> Duration.QuarterNote
            | "8" -> Duration.EighthNote
            | "16" -> Duration.SixteenthNote
            | _ -> failwith $"Unknown duration: \"{v}\""

    let pNote: P<Note> =
        parse {
            let! noteName = pNoteName
            let! duration = opt pDuration
            let! state = getUserState

            let lastNoteDuration = Option.map (_.Duration) state.LastNote

            let previousTimeSignatureDuration = state.PreviousTimeSignature.Denominator

            let duration =
                duration
                |> Option.orElse lastNoteDuration
                |> Option.defaultValue previousTimeSignatureDuration

            let note =
                { NoteName = noteName
                  Octave = 4
                  Duration = duration }

            do! updateUserState (fun s -> { s with LastNote = Some note })

            return note
        }

    let pPartDefinitionAttribute: P<_> =
        choice
            [ attempt <| command "id" .>> ws >>. num |>> PartDefinitionAttribute.Id
              attempt <| command "name" .>> ws >>. str |>> PartDefinitionAttribute.Name
              attempt <| command "clef" .>> ws >>. pclef |>> PartDefinitionAttribute.Clef
              attempt <| command "time" .>> ws >>. num .>>. spaces1 .>>. pDuration
              |>> fun ((numerator, _), denominator) ->
                  PartDefinitionAttribute.TimeSignature
                      { Numerator = numerator
                        Denominator = denominator }
              attempt <| command "key" .>> ws >>. pNoteName
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
                      Duration = Duration.EighthNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.EighthNote }
                |> withNote
                    { NoteName = NoteName.E
                      Octave = 4
                      Duration = Duration.EighthNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.EighthNote }

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
