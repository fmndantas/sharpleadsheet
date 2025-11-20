// For more information see https://aka.ms/fsharp-console-apps
open App

[<EntryPoint>]
let main argv =
  let source = argv[0]
  let result = source |> Workflow.Path.fromString |> Workflow.parse

  match result with
  | Ok m -> printfn "%A" m
  | Error _ -> printfn "Parsing generated errors"

  0 // return an integer exit code
