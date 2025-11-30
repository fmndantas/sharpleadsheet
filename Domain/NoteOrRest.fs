module Domain.NoteOrRest

open Domain.CommonTypes

let getDuration (n: NoteOrRest) : Duration.T =
  match n with
  | NoteOrRest.Note note -> Note.getDuration note
