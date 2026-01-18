module Domain.NoteOrRest

type T = {
  NoteOrRest: NoteOrRestChoiceType
  Modifiers: Modifier list
}

// TODO: think in a better name for this type
and NoteOrRestChoiceType =
  | Note of Note.T
  | Rest of Rest.T

and Modifier = exn

let getDuration (n: T) : Duration.T =
  match n.NoteOrRest with
  | Note note -> Note.getDuration note
  | Rest rest -> Rest.getDuration rest

let getChord (n: T) : Chord.T option =
  match n.NoteOrRest with
  | Note note -> Note.getChord note
  | Rest rest -> Rest.getChord rest

let getText (n: T) : string option =
  match n.NoteOrRest with
  | Note note -> Note.getText note
  | Rest rest -> Rest.getText rest

let isTied (n: T) : bool =
  match n.NoteOrRest with
  | Note note -> Note.isTied note
  | Rest _ -> false

let fromNote (n: Note.T) : T = { NoteOrRest = Note n; Modifiers = [] }

let fromRest (r: Rest.T) : T = { NoteOrRest = Rest r; Modifiers = [] }

let fold (noteF: Note.T -> 'a) (restF: Rest.T -> 'a) (n: T) : 'a =
  match n.NoteOrRest with
  | Note n -> noteF n
  | Rest r -> restF r

// TODO: remove
// let hasChord: NoteOrRest -> bool = getChord >> Option.isSome
