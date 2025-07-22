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
        { InitialTimeSignature: TimeSignature
          InitialKeySignature: KeySignature
          InitialClef: Clef
          LastNote: Note option
          LastMeasure: Measure option }

module Functions =
    open Types

    type private P<'a> = Parser<'a, ParsingState>

    [<AutoOpen>]
    module private Helpers =
        let ws = spaces
        let str: P<_> = many1Satisfy (System.Char.IsWhiteSpace >> not)
        let num: P<_> = pint32

    let command s = pchar ':' >>. pstring s

    let pclef: P<Clef> =
        [ "f"; "g" ] |> List.map pstring |> choice
        |>> fun v ->
            match v with
            | "g" -> Clef.G
            | _ -> failwith "Unknown clef: \"{v}\""

    let pNoteName: P<NoteName> =
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

    let pDuration: P<Duration> =
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

            let previousTimeSignatureDuration = state.InitialTimeSignature.Denominator

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

    // TODO: avoid backingtrack
    let pPartDefinitionAttribute: P<PartDefinitionAttribute> =
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

    let pPartDefinition: P<PartDefinition> =
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

    let pSequencesOfNotes: P<Measure list> =
        parse {
            let! notesByMeasures = sepBy (pNote .>> ws |> many) (pstring "|" .>> ws)
            let! state = getUserState

            let currentMeasureNumber =
                Option.map (_.MeasureNumber) state.LastMeasure
                |> Option.defaultValue (MeasureNumber 0)

            let keySignature = state.InitialKeySignature
            let timeSignature = state.InitialTimeSignature
            let clef = state.InitialClef

            let createMeasure =
                aMeasure
                >> withKeySignature keySignature
                >> withTimeSignature timeSignature
                >> withClef clef

            let _, updatedMeasures =
                notesByMeasures
                |> List.fold
                    (fun (MeasureNumber measureNumber, measures) notes ->
                        let updatedMeasureNumber = measureNumber + 1

                        let updatedMeasures =
                            List.append measures [ createMeasure updatedMeasureNumber |> withNotes notes ]

                        MeasureNumber updatedMeasureNumber, updatedMeasures)
                    (currentMeasureNumber, [])

            do!
                updateUserState (fun s ->
                    { s with
                        LastMeasure = List.tryLast updatedMeasures })

            return updatedMeasures
        }

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
