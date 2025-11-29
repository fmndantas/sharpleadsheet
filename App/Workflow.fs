module App.Workflow

open System.IO
open System.Xml.Linq

open FParsec

open Domain
open Domain.CommonTypes
open Domain.Parser.Types

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

let private defaultState = {
  InitialKeySignature = KeySignature NoteName.C
  InitialTimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  InitialClef = Clef.G
  CurrentOctave = 4
  LastDuration = None
  LastPitch = None
}

let parse: Parse =
  fun path ->
    let pathAsString = path |> Path.getPathAsString
    let inputText = pathAsString |> File.ReadAllText

    match runParserOnString Parser.Functions.pMusic defaultState pathAsString inputText with
    | Success(parsedMusic, _, _) -> Result.Ok parsedMusic
    | Failure(errorMessage, _, _) -> Result.Error errorMessage
    |> Result.mapError (WorkflowError.Parsing >> List.singleton)
    |> Result.bind (
      Validated.musicFromParsedMusic
      >> Result.mapError (List.map WorkflowError.Validation)
    )

// TODO: remove Music.Validated
let outputXml: OutputMusicXml =
  fun validatedMusic -> validatedMusic |> Music.Validated |> MusicToXml.convert
