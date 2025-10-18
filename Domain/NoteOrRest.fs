module Domain.NoteOrRest

open Domain.Types

let getDuration (n: NoteOrRest) : Duration =
  match n with
  | NoteOrRest.Note note -> Note.getDuration note
