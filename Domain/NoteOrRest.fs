module Domain.NoteOrRest

open CommonTypes

let getDuration (n: NoteOrRest) : Duration.T =
  match n with
  | NoteOrRest.Note note -> Note.getDuration note
  | NoteOrRest.Rest rest -> Rest.getDuration rest

let getChord (n: NoteOrRest) : Chord.T option =
  match n with
  | NoteOrRest.Note note -> Note.getChord note
  | NoteOrRest.Rest rest -> Rest.getChord rest

let getText (n: NoteOrRest) : string option =
  match n with
  | NoteOrRest.Note note -> Note.getText note
  | NoteOrRest.Rest rest -> Rest.getText rest

let isTied (n: NoteOrRest) : bool =
  match n with
  | NoteOrRest.Note note -> Note.isTied note
  | NoteOrRest.Rest _ -> false

// TODO: remove
// let hasChord: NoteOrRest -> bool = getChord >> Option.isSome
