module App.CliParser

open Argu

type CliArguments =
  | [<MainCommand; ExactlyOnce; First>] Sls_File of path: string
  | [<AltCommandLine("-o")>] Output_Directory of directory: string

  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Sls_File _ -> "specify the input file"
      | Output_Directory _ -> "specify the output directory"

let createParser programName errorHandler =
  ArgumentParser.Create<CliArguments>(programName = programName, errorHandler = errorHandler)
