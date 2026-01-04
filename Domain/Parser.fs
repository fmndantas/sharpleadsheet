module Domain.Parser

open FParsec

open CommonTypes
open ParsedTypes
open ParsedMeasureBuilder
open ParserStateBuilder

[<Literal>]
let bypassDebug = true

[<AutoOpen>]
module Types =
  [<RequireQualifiedAccess>]
  type PartDefinitionAttribute =
    | Id of int
    | Name of string
    | Clef of Clef
    | TimeSignature of TimeSignature
    | KeySignature of KeySignature
    | Comment

  [<RequireQualifiedAccess>]
  type NotesSectionSymbol =
    | Note of Note.T
    | Rest of Rest.T
    | OctaveManipulation of int
    | Chord of Chord.T
    | Comment

module Functions =
  open Types

  type private P<'a> = Parser<'a, ParserState>

  [<AutoOpen>]
  module private Debug =
    let (<!>) (p: P<_>) label : P<_> =
      if bypassDebug then
        p
      else
        fun stream ->
          printfn "[DEBUG] %A: Entering %s" stream.Position label
          let reply = p stream
          printfn "[DEBUG] %A: Leaving %s (%A)" stream.Position label reply.Status
          reply

  [<AutoOpen>]
  module private Helpers =
    let pComment: P<_> = pchar '#' >>. skipRestOfLine true <!> "pComment"
    let ws: P<_> = spaces <!> "ws"
    let ws1: P<_> = spaces1 <!> "ws1"
    let ws1OrComments: P<_> = spaces1 <|> pComment |> many <!> "ws1OrComments"
    let str: P<_> = many1Satisfy (System.Char.IsWhiteSpace >> not)
    let num: P<_> = pint32

  let pCommand s =
    pchar ':' >>. pstring s <?> sprintf ":%s" s <!> $"pCommand {s}"

  let pEndCommand s =
    pstring s >>. pchar ':' <?> sprintf "%s:" s <!> $"pEndCommand {s}"

  let pCommandWithBacktrack s = pchar ':' >>? pstring s

  let pclef: P<Clef> =
    [ "f"; "g" ] |> List.map pstring |> choice
    |>> fun v ->
      match v with
      | "g" -> Clef.G
      | "f" -> Clef.F
      | _ -> failwith "Unknown clef: \"{v}\""

  let pNoteName: P<NoteName.T> =
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

  let pDuration: P<Duration.T> =
    [ "32"; "16."; "16"; "8."; "8"; "4."; "4"; "2."; "2"; "1."; "1" ]
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
      | "32" -> Duration.ThirtySecond
      | _ -> failwith $"Unknown duration: \"{v}\""

  let getUpdatedDuration (state: ParserState) (maybeNewDuration: Duration.T option) =
    maybeNewDuration
    |> Option.orElse state.LastDuration
    |> Option.defaultValue state.CurrentTimeSignature.Denominator

  let pTie: P<Note.Modifier> = pstring "~" |>> fun _ -> Note.Tie

  let pNote: P<Note.T> =
    parse {
      let! noteName = pNoteName
      let! maybeParsedDuration = opt pDuration
      let! state = getUserState
      let duration = getUpdatedDuration state maybeParsedDuration
      let! maybeTie = opt pTie

      let note =
        Note.create' state.CurrentOctave (List.choose id [ maybeTie ]) noteName duration
        |> Note.maybeWithChord state.LastChord

      do!
        updateUserState (fun s -> {
          s with
              LastPitch = note |> Note.getPitch |> Some
              LastDuration = note |> Note.getDuration |> Some
              LastChord = None
        })

      return note
    }

  let pRest: P<Rest.T> =
    parse {
      let! state = getUserState
      let! maybeDuration = pstring "r" >>. opt pDuration
      let duration = getUpdatedDuration state maybeDuration

      do!
        updateUserState (fun s -> {
          s with
              LastDuration = Some duration
              LastChord = None
        })

      return Rest.create duration |> Rest.maybeWithChord state.LastChord
    }

  let pPartDefinitionAttribute: P<PartDefinitionAttribute> =
    choice [
      pCommandWithBacktrack "id" .>> ws1 >>. num |>> PartDefinitionAttribute.Id
      pCommandWithBacktrack "name" .>> ws1 >>. str |>> PartDefinitionAttribute.Name
      pCommandWithBacktrack "clef" .>> ws1 >>. pclef |>> PartDefinitionAttribute.Clef
      pCommandWithBacktrack "time" .>> ws1 >>. num .>>. spaces1 .>>. pDuration
      |>> fun ((numerator, _), denominator) ->
        PartDefinitionAttribute.TimeSignature {
          Numerator = numerator
          Denominator = denominator
        }
      pCommandWithBacktrack "key" .>> ws1 >>. pNoteName
      |>> fun v -> v |> KeySignature |> PartDefinitionAttribute.KeySignature
      pComment |>> fun _ -> PartDefinitionAttribute.Comment
    ]
    <!> "pPartDefinitionAttribute"

  let pPartDefinitionSection (settings: DefaultSettings) : P<ParsedPartDefinitionSection> =
    between (pCommand "part" .>> ws) (pEndCommand "part" .>> ws) (many (pPartDefinitionAttribute .>> ws))
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
        | PartDefinitionAttribute.KeySignature v -> keySignature <- Some v
        | _ -> ())

      {
        Id = partId
        Name = name
        TimeSignature = timeSignature |> Option.defaultValue settings.TimeSignature
        KeySignature = keySignature |> Option.defaultValue settings.KeySignature
        Clef = clef |> Option.defaultValue settings.Clef
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

  let pChord: P<Chord.T> =
    parse {
      let pChordKind = manySatisfy (fun c -> c <> ']' && c <> '/')
      let! root = pNoteName
      let! maybeKind = opt (pchar '.' >>. pChordKind)
      let! maybeBass = opt (pchar '/' >>. pNoteName)
      let chord = Chord.create root maybeBass maybeKind
      do! updateUserState (fun s -> { s with LastChord = Some chord })
      return chord
    }

  let pNotesSectionContent: P<ParsedMeasure list> =
    let pSymbol: P<NotesSectionSymbol> =
      choice [
        pNote <!> "pNote" |>> NotesSectionSymbol.Note .>> ws1
        pRest |>> NotesSectionSymbol.Rest .>> ws1
        pOctaveManipulation .>> ws1
        between (pchar '[') (pchar ']') pChord |>> NotesSectionSymbol.Chord .>> ws1
        (pComment |>> fun _ -> NotesSectionSymbol.Comment) .>> ws
      ]

    parse {
      let! symbolsPerMeasure = sepBy (many pSymbol) (pstring "|" <!> "bar" .>> ws)

      let symbolsPerMeasure: NoteOrRest list list =
        symbolsPerMeasure
        |> List.map (
          List.choose (function
            | NotesSectionSymbol.Note note -> note |> NoteOrRest.Note |> Some
            | NotesSectionSymbol.Rest rest -> rest |> NoteOrRest.Rest |> Some
            | _ -> None)
        )

      let! state = getUserState

      let keySignature = state.CurrentKeySignature
      let timeSignature = state.CurrentTimeSignature
      let clef = state.CurrentClef

      let createMeasure symbols =
        aParsedMeasure ()
        |> withKeySignature keySignature
        |> withTimeSignature timeSignature
        |> withClef clef
        |> withSymbols symbols

      let updatedMeasures =
        symbolsPerMeasure
        |> List.filter (List.isEmpty >> not)
        |> List.fold (fun acc symbols -> List.append acc [ createMeasure symbols ]) []

      return updatedMeasures
    }

  let pNotesSection: P<ParsedNotesSection> =
    parse {
      let! partId = pCommand "notes" >>. ws1 >>. (pint32 <?> "part id") .>> ws1OrComments |>> PartId
      let! sequenceOfNotes = pNotesSectionContent
      let! _ = pEndCommand "notes" .>> ws1OrComments

      return {
        PartId = partId
        Measures = sequenceOfNotes
      }
    }

  let pMusic (settings: DefaultSettings) : P<ParsedMusic> =
    parse {
      let! partDefinition = ws1OrComments >>. pPartDefinitionSection settings .>> ws1OrComments

      do!
        aParserState ()
        |> withCurrentTimeSignature partDefinition.TimeSignature
        |> withCurrentKeySignature partDefinition.KeySignature
        |> withCurrentClef partDefinition.Clef
        |> withCurrentOctave 4
        |> withoutLastPitch
        |> withoutLastDuration
        |> withoutLastChord
        |> setUserState

      let! notesSection = many1 pNotesSection

      return {
        PartDefinitionSections = [ partDefinition ]
        NotesSections =
          notesSection
          |> List.groupBy _.PartId
          |> List.map (fun (partId, sections) -> {
            PartId = partId
            Measures = sections |> List.collect _.Measures
          })
      }
    }
