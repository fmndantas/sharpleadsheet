// For more information see https://aka.ms/fsharp-console-apps
open FSharp.Core.Result

open App

[<EntryPoint>]
let main argv =
  let source = argv[0]
  let result = source |> Workflow.Path.fromString |> Workflow.parseAndShow

  match result with
  | Ok m -> printfn "%A" m
  | Error e -> printfn "%s" e

  0 // return an integer exit code
