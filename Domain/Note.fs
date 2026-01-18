module Domain.Note

open GenericFunctions

type T = {
  Pitch: Pitch.T
  Duration: Duration.T
  Modifiers: Modifier list
}

// TODO: just one modifier of each type?
and Modifier =
  | Text of string
  | Chord of Chord.T

[<AutoOpen>]
module private Helper =
  let addModifier (m: Modifier) (note: T) : T = {
    note with
        Modifiers = m :: note.Modifiers
  }

  // TODO: This is duplicated in Rest.fs
  let maybeGetModifier<'a> (m: Modifier -> bool) (f: Modifier -> 'a option) (note: T) : 'a option =
    note.Modifiers |> List.tryFind m |> Option.bind f

  // TODO: This is duplicated in Rest.fs
  let maybeAddModifier (m: Modifier option) (note: T) : T =
    m |> Option.map (note |> flip2 addModifier) |> Option.defaultValue note

let create (octave: int) (noteName: NoteName.T) (duration: Duration.T) : T = {
  Pitch = Pitch.create noteName octave
  Duration = duration
  Modifiers = []
}

let create2 = create 2

let create4 = create 4

let create5 = create 5

let create6 = create 6

let getPitch (note: T) : Pitch.T = note.Pitch

let getDuration (note: T) : Duration.T = note.Duration

let getText (note: T) : string option =
  note
  |> maybeGetModifier _.IsText (function
    | Text t -> Some t
    | _ -> None)

let withText (text: string) (note: T) : T = addModifier (Text text) note

let maybeWithText (text: string option) (note: T) : T =
  maybeAddModifier (Option.map Text text) note
