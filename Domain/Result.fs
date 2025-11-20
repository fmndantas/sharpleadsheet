[<RequireQualifiedAccess>]
module Domain.Result

let apply f r =
  match f, r with
  | Ok fn, Ok v -> v |> fn |> Ok
  | Ok _, Error es
  | Error es, Ok _ -> Error es
  | Error es1, Error es2 -> Error [ yield! es1; yield! es2 ]
