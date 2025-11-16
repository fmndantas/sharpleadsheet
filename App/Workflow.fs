module App.Workflow

open System.IO

open FParsec

open Domain
open Domain.Types
open Domain.Parser.Types

module Path =
  type T = private Path of string

  let fromString (s: string) = Path s

  let getPathAsString (v: T) =
    match v with
    | Path s -> s

type ParseAndShow = Path.T -> Result<Music, string>

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

let parseAndShow: ParseAndShow =
  fun path ->
    let pathAsString = path |> Path.getPathAsString
    let inputText = pathAsString |> File.ReadAllText

    match runParserOnString Parser.Functions.pMusic defaultState pathAsString inputText with
    | Success(result, _, _) -> Result.Ok result
    | Failure(errorMessage, _, _) -> Result.Error errorMessage
    |> Result.bind Validator.validate
