[<RequireQualifiedAccess>]
module Domain.Result

let apply f r =
  match f, r with
  | Ok fn, Ok v -> v |> fn |> Ok
  | Ok _, Error es
  | Error es, Ok _ -> Error es
  | Error es1, Error es2 -> Error [ yield! es1; yield! es2 ]

let traverse (rs: Result<'a, 'b list> list) : Result<'a list, 'b list> =
  let folder r acc =
    match r, acc with
    | Ok v, Ok vs -> v :: vs |> Ok
    | Ok _, Error es
    | Error es, Ok _ -> Error es
    | Error es1, Error es2 -> Error [ yield! es1; yield! es2 ]

  List.foldBack folder rs (Ok [])
