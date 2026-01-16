module Domain.Note

open GenericFunctions

type T = {
  Pitch: Pitch.T
  Duration: Duration.T
  Modifiers: Modifier list
}

and Modifier =
  | Tie
  | Text of string
  | Chord of Chord.T

[<AutoOpen>]
module private Helper =
  /// Create a note with modifiers
  let create' octave modifiers noteName duration = {
    Pitch = Pitch.create noteName octave
    Duration = duration
    Modifiers = modifiers
  }

  let addModifier (m: Modifier) (note: T) : T = {
    note with
        Modifiers = m :: note.Modifiers
  }

  let maybeGetModifier<'a> (m: Modifier -> bool) (f: Modifier -> 'a option) (note: T) : 'a option =
    note.Modifiers |> List.tryFind m |> Option.bind f

  let maybeAddModifier (m: Modifier option) (note: T) : T =
    m |> Option.map (note |> flip2 addModifier) |> Option.defaultValue note

/// Create a note without modifiers
let create (octave: int) (noteName: NoteName.T) (duration: Duration.T) : T = {
  Pitch = Pitch.create noteName octave
  Duration = duration
  Modifiers = []
}

let create2 = create 2

let create4 = create 4

let createTied4 = create' 4 [ Tie ]

let create5 = create 5

let create6 = create 6

let getPitch (note: T) : Pitch.T = note.Pitch

let getDuration (note: T) : Duration.T = note.Duration

let getChord (note: T) : Chord.T option =
  note
  |> maybeGetModifier _.IsChord (function
    | Chord c -> Some c
    | _ -> None)

let isTied (note: T) : bool = note.Modifiers |> List.contains Tie

let withChord (chord: Chord.T) (note: T) : T = addModifier (Chord chord) note

let maybeWithChord (chord: Chord.T option) (note: T) : T =
  maybeAddModifier (Option.map Chord chord) note

let withText (text: string) (note: T) : T = addModifier (Text text) note

let maybeWithText (text: string option) (note: T) : T =
  maybeAddModifier (Option.map Text text) note

let maybeWithTie (tie: unit option) (note: T) : T =
  maybeAddModifier (Option.map (fun _ -> Tie) tie) note
