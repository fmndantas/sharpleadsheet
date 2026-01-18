module Domain.Rest

type T = {
  Duration: Duration.T
  Modifiers: Modifier list
}

// TODO: just one modifier of each type
and Modifier =
  | Text of string
  | Chord of Chord.T

[<AutoOpen>]
module private Helper =
  let addModifier (m: Modifier) (rest: T) : T = {
    rest with
        Modifiers = m :: rest.Modifiers
  }

  let maybeGetModifier<'a> (m: Modifier -> bool) (f: Modifier -> 'a option) (rest: T) : 'a option =
    rest.Modifiers |> List.tryFind m |> Option.bind f

  let maybeAddModifier (m: Modifier option) (rest: T) : T =
    m |> Option.map (rest |> flip2 addModifier) |> Option.defaultValue rest

let create (duration: Duration.T) : T = { Duration = duration; Modifiers = [] }

let getDuration ({ Duration = duration }: T) : Duration.T = duration

let getText (rest: T) : string option =
  rest
  |> maybeGetModifier _.IsText (function
    | Text t -> Some t
    | _ -> None)

let withText (text: string) (rest: T) : T = addModifier (Text text) rest

let maybeWithText (text: string option) (rest: T) : T =
  maybeAddModifier (Option.map Text text) rest
