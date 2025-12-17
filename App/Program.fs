// For more information see https://aka.ms/fsharp-console-apps
open System

open Argu

open App

[<EntryPoint>]
let main argv =

  let errorHandler =
    ProcessExiter(
      colorizer =
        function
        | ErrorCode.HelpText -> None
        | _ -> Some ConsoleColor.Red
    )

  let parser = CliParser.createParser "sharpleadsheet" errorHandler

  parser.ParseCommandLine argv |> Workflow.run

  0 // return an integer exit code
