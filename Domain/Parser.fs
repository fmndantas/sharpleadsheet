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

  [<RequireQualifiedAccess>]
  type NotesSectionSymbol =
    | Note of Note.T
    | Rest of Rest
    | OctaveManipulation of int

  type PartDefinitionSection = {
    Id: PartId option
    Name: string option
    Clef: Clef option
    TimeSignature: TimeSignature option
    KeySignature: KeySignature option
  }

  type NotesSection = {
    PartId: PartId
    Measures: Measure list
  }

  type ParserState = {
    InitialTimeSignature: TimeSignature
    InitialKeySignature: KeySignature
    InitialClef: Clef
    CurrentOctave: int
    LastPitch: Pitch option
    LastDuration: Duration option
    LastMeasureId: MeasureId option
  }

module Functions =
  open Types

  type private P<'a> = Parser<'a, ParserState>

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
    [
      "cs"
      "c"
      "df"
      "ds"
      "d"
      "ef"
      "e"
      "fs"
      "f"
      "gf"
      "gs"
      "g"
      "af"
      "as"
      "a"
      "bf"
      "b"
    ]
    |> List.map pstring
    |> choice
    |>> fun v ->
      match v with
      | "c" -> NoteName.C
      | "cs" -> NoteName.CSharp
      | "df" -> NoteName.DFlat
      | "d" -> NoteName.D
      | "ds" -> NoteName.DSharp
      | "ef" -> NoteName.EFlat
      | "e" -> NoteName.E
      | "f" -> NoteName.F
      | "fs" -> NoteName.FSharp
      | "gf" -> NoteName.GFlat
      | "g" -> NoteName.G
      | "gs" -> NoteName.GSharp
      | "af" -> NoteName.AFlat
      | "a" -> NoteName.A
      | "as" -> NoteName.ASharp
      | "bf" -> NoteName.BFlat
      | "b" -> NoteName.B
      | _ -> failwith $"Unknown note name: \"{v}\""

  let pDuration: P<Duration> =
    [ "16."; "16"; "8."; "8"; "4."; "4"; "2."; "2"; "1."; "1" ]
    |> List.map pstring
    |> choice
    |>> fun v ->
      match v with
      | "1" -> Duration.Whole
      | "1." -> Duration.WholeDotted
      | "2" -> Duration.Half
      | "2." -> Duration.HalfDotted
      | "4" -> Duration.Quarter
      | "4." -> Duration.QuarterDotted
      | "8" -> Duration.Eighth
      | "8." -> Duration.EighthDotted
      | "16" -> Duration.Sixteenth
      | "16." -> Duration.SixteenthDotted
      | _ -> failwith $"Unknown duration: \"{v}\""

  let getUpdatedDuration (state: ParserState) (maybeNewDuration: Duration option) =
    maybeNewDuration
    |> Option.orElse state.LastDuration
    |> Option.defaultValue state.InitialTimeSignature.Denominator

  let pNote: P<Note.T> =
    parse {
      let! noteName = pNoteName
      let! maybeDuration = opt pDuration
      let! state = getUserState
      let duration = getUpdatedDuration state maybeDuration
      let note = Note.create noteName state.CurrentOctave duration

      do!
        updateUserState (fun s -> {
          s with
              LastPitch = note |> Note.getPitch |> Some
              LastDuration = note |> Note.getDuration |> Some
        })

      return note
    }

  let pRest: P<Rest> =
    parse {
      let! state = getUserState
      let! maybeDuration = pstring "r" >>. opt pDuration
      let duration = getUpdatedDuration state maybeDuration

      do! updateUserState (fun s -> { s with LastDuration = Some duration })
      return Rest duration
    }

  let pPartDefinitionAttribute: P<PartDefinitionAttribute> =
    choice [
      pCommandWithBacktrack "id" .>> ws >>. num |>> PartDefinitionAttribute.Id
      pCommandWithBacktrack "name" .>> ws >>. str |>> PartDefinitionAttribute.Name
      pCommandWithBacktrack "clef" .>> ws >>. pclef |>> PartDefinitionAttribute.Clef
      pCommandWithBacktrack "time" .>> ws >>. num .>>. spaces1 .>>. pDuration
      |>> fun ((numerator, _), denominator) ->
        PartDefinitionAttribute.TimeSignature {
          Numerator = numerator
          Denominator = denominator
        }
      pCommandWithBacktrack "key" .>> ws >>. pNoteName
      |>> fun v -> v |> KeySignature |> PartDefinitionAttribute.KeySignature
    ]

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
        | PartDefinitionAttribute.Id v -> partId <- v |> PartId |> Some
        | PartDefinitionAttribute.Name v -> name <- Some v
        | PartDefinitionAttribute.Clef v -> clef <- Some v
        | PartDefinitionAttribute.TimeSignature v -> timeSignature <- Some v
        | PartDefinitionAttribute.KeySignature v -> keySignature <- Some v)

      {
        Id = partId
        Name = name
        Clef = clef
        TimeSignature = timeSignature
        KeySignature = keySignature
      })

  let pOctaveManipulation: P<NotesSectionSymbol> =
    parse {
      let! operation, maybeDelta = choice [ pstring "o+" .>>. opt num; pstring "o-" .>>. opt num ]
      let! state = getUserState

      let delta = Option.defaultValue 1 maybeDelta

      let updatedOctave =
        state.CurrentOctave
        + match operation with
          | "o+" -> delta
          | "o-" -> -delta
          | _ -> 0

      do! updateUserState (fun s -> { s with CurrentOctave = updatedOctave })
      return NotesSectionSymbol.OctaveManipulation updatedOctave
    }

  let pNotesSectionContent: P<Measure list> =
    parse {
      let pSymbol: P<NotesSectionSymbol> =
        choice [
          pNote |>> NotesSectionSymbol.Note
          pRest |>> NotesSectionSymbol.Rest
          pOctaveManipulation
        ]

      let! symbolsPerMeasure = sepBy (pSymbol .>> ws |> many) (pstring "|" .>> ws)

      let symbolsPerMeasure: NoteOrRest list list =
        symbolsPerMeasure
        |> List.map (
          List.choose (function
            | NotesSectionSymbol.Note note -> note |> NoteOrRest.Note |> Some
            | NotesSectionSymbol.Rest rest -> rest |> NoteOrRest.Rest |> Some
            | _ -> None)
        )

      let! state = getUserState

      let currentMeasureId = Option.defaultValue (MeasureId 0) state.LastMeasureId

      let keySignature = state.InitialKeySignature
      let timeSignature = state.InitialTimeSignature
      let clef = state.InitialClef

      let createMeasure =
        aMeasure
        >> withKeySignature keySignature
        >> withTimeSignature timeSignature
        >> withClef clef

      let _, updatedMeasures =
        symbolsPerMeasure
        |> List.filter (List.isEmpty >> not)
        |> List.fold
          (fun (MeasureId id, acc) symbols ->
            let updatedId = id + 1

            let updatedMeasures =
              List.append acc [ createMeasure updatedId |> withSymbols symbols ]

            MeasureId updatedId, updatedMeasures)
          (currentMeasureId, [])

      do!
        updateUserState (fun s -> {
          s with
              LastMeasureId = List.tryLast updatedMeasures |> Option.map _.Id
        })

      return updatedMeasures
    }

  let pNotesSection: P<NotesSection> =
    parse {
      let! partId = pCommand "notes" >>. ws >>. pint32 .>> ws |>> PartId
      let! sequenceOfNotes = pNotesSectionContent
      let! _ = pCommand "endnotes" .>> ws

      return {
        PartId = partId
        Measures = sequenceOfNotes
      }
    }

  // TODO: validation (to remove Option.get)
  let pMusic: P<Music> =
    parse {
      let! partDefinition = pPartDefinitionSection

      do!
        setUserState {
          InitialTimeSignature = Option.get partDefinition.TimeSignature
          InitialKeySignature = Option.get partDefinition.KeySignature
          InitialClef = Option.get partDefinition.Clef
          CurrentOctave = 4
          LastPitch = None
          LastDuration = None
          LastMeasureId = None
        }

      let! notesSection = many1 pNotesSection

      let parts =
        notesSection
        |> List.groupBy _.PartId
        |> List.map (fun (partId, notesSection) -> partId, notesSection |> List.map _.Measures |> List.concat)
        |> List.map (fun (partId, measures) -> {
          Id = partId
          Name = Option.get partDefinition.Name
          Measures = measures
        })

      return Music parts
    }
