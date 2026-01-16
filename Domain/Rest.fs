module Domain.Rest

type T = {
  Duration: Duration.T
  Modifiers: Modifier list
}

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

let getChord (rest: T) : Chord.T option =
  rest
  |> maybeGetModifier _.IsChord (function
    | Chord c -> Some c
    | _ -> None)

let withChord (chord: Chord.T) (rest: T) : T = addModifier (Chord chord) rest

let maybeWithChord (chord: Chord.T option) (rest: T) : T =
  maybeAddModifier (Option.map Chord chord) rest

let withText (text: string) (rest: T) : T = addModifier (Text text) rest

let maybeWithText (text: string option) (rest: T) : T =
  maybeAddModifier (Option.map Text text) rest
