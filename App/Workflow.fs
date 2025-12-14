module App.Workflow

open System.IO
open System.Xml.Linq

open FParsec

open Domain
open CommonTypes
open ParsedTypes

module Path =
  type T = private Path of string

  let fromString (s: string) = Path s

  let getPathAsString (v: T) =
    match v with
    | Path s -> s

[<RequireQualifiedAccess>]
type WorkflowError =
  | Parsing of string
  | Validation of ValidationError

type Parse = Path.T -> Result<Validated.Music, WorkflowError list>
type OutputMusicXml = Validated.Music -> XDocument

let private defaultSettings = {
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  Clef = Clef.G
}

let private defaultState = {
  CurrentKeySignature = defaultSettings.KeySignature
  CurrentTimeSignature = defaultSettings.TimeSignature
  CurrentClef = defaultSettings.Clef
  CurrentOctave = 4
  LastDuration = None
  LastPitch = None
}

let parse: Parse =
  fun path ->
    let pathAsString = path |> Path.getPathAsString
    let inputText = pathAsString |> File.ReadAllText

    let pMusic = Parser.Functions.pMusic defaultSettings

    match runParserOnString pMusic defaultState pathAsString inputText with
    | Success(parsedMusic, _, _) -> Result.Ok parsedMusic
    | Failure(errorMessage, _, _) -> Result.Error errorMessage
    |> Result.mapError (WorkflowError.Parsing >> List.singleton)
    |> Result.bind (
      Validated.musicFromParsedMusic
      >> Result.mapError (List.map WorkflowError.Validation)
    )
