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

    type PartDefinitionSection =
        { Id: PartId option
          Name: string option
          Clef: Clef option
          TimeSignature: TimeSignature option
          KeySignature: KeySignature option }

    type NotesSection =
        { PartId: PartId
          Measures: Measure list }

    type ParsingState =
        { InitialTimeSignature: TimeSignature
          InitialKeySignature: KeySignature
          InitialClef: Clef
          LastNote: Note option
          LastMeasureNumber: MeasureNumber option }

module Functions =
    open Types

    type private P<'a> = Parser<'a, ParsingState>

    [<AutoOpen>]
    module private Helpers =
        let ws = spaces
        let str: P<_> = many1Satisfy (System.Char.IsWhiteSpace >> not)
        let num: P<_> = pint32

    let pCommand s = pchar ':' >>. pstring s

    let pCommandWithBacktrack s = pchar ':' >>? pstring s

    let pclef: P<Clef> =
        [ "f"; "g" ] |> List.map pstring |> choice
        |>> fun v ->
            match v with
            | "g" -> Clef.G
            | "f" -> Clef.F
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
            | "1" -> Duration.Whole
            | "2" -> Duration.Half
            | "4" -> Duration.Quarter
            | "8" -> Duration.Eighth
            | "16" -> Duration.Sixteenth
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

    let pPartDefinitionAttribute: P<PartDefinitionAttribute> =
        choice
            [ pCommandWithBacktrack "id" .>> ws >>. num |>> PartDefinitionAttribute.Id
              pCommandWithBacktrack "name" .>> ws >>. str |>> PartDefinitionAttribute.Name
              pCommandWithBacktrack "clef" .>> ws >>. pclef |>> PartDefinitionAttribute.Clef
              pCommandWithBacktrack "time" .>> ws >>. num .>>. spaces1 .>>. pDuration
              |>> fun ((numerator, _), denominator) ->
                  PartDefinitionAttribute.TimeSignature
                      { Numerator = numerator
                        Denominator = denominator }
              pCommandWithBacktrack "key" .>> ws >>. pNoteName
              |>> fun v -> v |> KeySignature |> PartDefinitionAttribute.KeySignature ]

    let pPartDefinitionSection: P<PartDefinitionSection> =
        between (pCommand "part" .>> ws) (pCommand "endpart" .>> ws) (many (pPartDefinitionAttribute .>> ws))
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
            let! notesPerMeasure = sepBy (pNote .>> ws |> many) (pstring "|" .>> ws)
            let! state = getUserState

            let currentMeasureNumber =
                Option.defaultValue (MeasureNumber 0) state.LastMeasureNumber

            let keySignature = state.InitialKeySignature
            let timeSignature = state.InitialTimeSignature
            let clef = state.InitialClef

            let createMeasure =
                aMeasure
                >> withKeySignature keySignature
                >> withTimeSignature timeSignature
                >> withClef clef

            let _, updatedMeasures =
                notesPerMeasure
                |> List.filter (List.isEmpty >> not)
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
                        LastMeasureNumber = List.tryLast updatedMeasures |> Option.map (_.MeasureNumber) })

            return updatedMeasures
        }

    let pNotesSection: P<NotesSection> =
        parse {
            let! partId = pCommand "notes" >>. ws >>. pint32 .>> ws |>> PartId
            let! sequenceOfNotes = pSequencesOfNotes
            let! _ = pCommand "endnotes" .>> ws

            return
                { PartId = partId
                  Measures = sequenceOfNotes }
        }

    // TODO: validation (to remove Option.get)
    let pMusic: P<Music> =
        parse {
            let! partDefinition = pPartDefinitionSection

            do!
                setUserState
                    { InitialTimeSignature = Option.get partDefinition.TimeSignature
                      InitialKeySignature = Option.get partDefinition.KeySignature
                      InitialClef = Option.get partDefinition.Clef
                      LastNote = None
                      LastMeasureNumber = None }

            let! notesSection = many1 pNotesSection

            let parts =
                notesSection
                |> List.groupBy _.PartId
                |> List.map (fun (partId, notesSection) -> partId, notesSection |> List.map _.Measures |> List.concat)
                |> List.map (fun (partId, measures) ->
                    { Id = partId
                      Name = Option.get partDefinition.Name
                      Measures = measures })

            return Music parts
        }
