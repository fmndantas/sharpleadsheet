module Domain.NoteOrRest

type T = {
  NoteOrRest: NoteOrRestChoiceType
  Modifiers: Modifier list
}

// TODO: think in a better name for this type
and NoteOrRestChoiceType =
  | Note of Note.T
  | Rest of Rest.T

and Modifier =
  | Tie
  | Chord of Chord.T

let fromNote (n: Note.T) : T = { NoteOrRest = Note n; Modifiers = [] }

let fromRest (r: Rest.T) : T = { NoteOrRest = Rest r; Modifiers = [] }

// TODO: create addModifier

let withTie (n: T) : T = {
  n with
      Modifiers = Tie :: n.Modifiers
}

let withChord (chord: Chord.T) (n: T) : T = {
  n with
      Modifiers = Chord chord :: n.Modifiers
}

let withChordOption (chord: Chord.T option) (n: T) : T =
  chord |> Option.map (n |> (withChord |> flip2)) |> Option.defaultValue n

let fromNoteWithTie: Note.T -> T = fromNote >> withTie

let getDuration (n: T) : Duration.T =
  match n.NoteOrRest with
  | Note note -> Note.getDuration note
  | Rest rest -> Rest.getDuration rest

let getChord (n: T) : Chord.T option =
  n.Modifiers
  |> List.tryFind _.IsChord
  |> Option.bind (function
    | Chord v -> Some v
    | _ -> None)

let getText (n: T) : string option =
  match n.NoteOrRest with
  | Note note -> Note.getText note
  | Rest rest -> Rest.getText rest

let isTied (n: T) : bool =
  n.Modifiers
  |> List.exists (function
    | Tie -> true)

let fold (noteF: Note.T -> 'a) (restF: Rest.T -> 'a) (n: T) : 'a =
  match n.NoteOrRest with
  | Note n -> noteF n
  | Rest r -> restF r
