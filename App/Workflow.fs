module App.Workflow

open System
open System.IO
open System.Xml.Linq

open FParsec
open Argu

open Domain
open CommonTypes
open ParsedTypes
open ParserStateBuilder

open App.CliParser

type Path = string
type SlsPath = string
type XmlPath = string
type PdfPath = string

module WorkflowError =
  type T =
    | Parsing of string
    | Validation of ValidationError

  let private partIdToString =
    function
    | PartId v -> v

  let private measureIdToString =
    function
    | MeasureId v -> v

  let workflowErrorToString (e: T) =
    match e with
    | Parsing s -> s
    | Validation v ->
      (match v with
       | ValidationError.PartDefinitionMissingId index -> sprintf "Part %d: missing id" index
       | ValidationError.PartDefinitionMissingName index -> sprintf "Part %d: missing name" index
       | ValidationError.PartDefinitionsWithRepeatedIds { PartId = partId; Indexes = indexes } ->
         sprintf
           "Duplicate part definition with ID %d found at the following positions: %A"
           (partIdToString partId)
           indexes
       | ValidationError.NotesSectionReferencesInvalidPartId { PartId = partId; Index = index } ->
         sprintf "Notes section %d: reference to invalid part id \"%d\"" index (partIdToString partId)
       | ValidationError.MeasureWithInconsistentDurations(measureId, partId) ->
         sprintf
           "Measure %d in part %d has inconsistent durations"
           (measureIdToString measureId)
           (partIdToString partId))
      |> sprintf "%2s -> %s" String.Empty

type Parse = Path -> Result<Validated.Music, WorkflowError.T list>
type ConvertValidatedMusicToXml = Validated.Music -> XDocument
type SaveXmlFile = SlsPath -> Path -> XDocument -> XmlPath

let private defaultSettings = {
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  Clef = Clef.G
}

let private defaultState =
  aParserState ()
  |> withCurrentKeySignature defaultSettings.KeySignature
  |> withCurrentTimeSignature defaultSettings.TimeSignature
  |> withCurrentClef defaultSettings.Clef
  |> withCurrentOctave 4
  |> withoutLastDuration
  |> withoutLastPitch
  |> withoutLastChord

let private parse: Parse =
  fun path ->
    let inputText = path |> File.ReadAllText
    let pMusic = Parser.Functions.pMusic defaultSettings

    match runParserOnString pMusic defaultState path inputText with
    | Success(parsedMusic, _, _) -> Result.Ok parsedMusic
    | Failure(errorMessage, _, _) -> Result.Error errorMessage
    |> Result.mapError (WorkflowError.Parsing >> List.singleton)
    |> Result.bind (
      Validated.musicFromParsedMusic
      >> Result.mapError (List.map WorkflowError.Validation)
    )

let private convertValidatedMusicToXml: ConvertValidatedMusicToXml =
  MusicToXml.convert

let private saveXmlFile: SaveXmlFile =
  fun slsFile directory xml ->
    let slsFileWithoutExtension = Path.GetFileNameWithoutExtension slsFile
    let outputFilename = slsFileWithoutExtension + ".xml"

    let outputFile = Path.Combine [| directory; outputFilename |]

    File.WriteAllText(outputFile, xml.ToString())

    outputFile

let printfnColoredText (color: ConsoleColor) (text: string) =
  let originalColor = Console.ForegroundColor

  try
    Console.ForegroundColor <- color
    printfn "%s" text
  finally
    Console.ForegroundColor <- originalColor

let printfnSuccess text =
  printfnColoredText ConsoleColor.Green text

let printfnWarning text =
  printfnColoredText ConsoleColor.Yellow text

let printfnError text =
  printfnColoredText ConsoleColor.Red text

let run (arguments: ParseResults<CliParser.CliArguments>) =
  let slsFile = arguments.GetResult Sls_File
  let outputDirectory = arguments.GetResult Output_Directory

  let result =
    slsFile
    |> parse
    |> Result.map convertValidatedMusicToXml
    |> Result.map (saveXmlFile slsFile outputDirectory)

  match result with
  | Result.Ok outputFile -> printfnSuccess (sprintf "Parsing went OK! Output was saved in \"%s\"" outputFile)
  | Result.Error errors ->
    let text =
      String.Join(
        Environment.NewLine,
        [|
          "Parsing generated errors :("
          yield! List.map WorkflowError.workflowErrorToString errors
        |]
      )

    printfnError text
