module Domain.NoteOrRest

open Domain.CommonTypes

let getDuration (n: NoteOrRest) : Duration.T =
  match n with
  | NoteOrRest.Note note -> Note.getDuration note
  | NoteOrRest.Rest(Rest d) -> d

let isTied (n: NoteOrRest) : bool =
  match n with
  | NoteOrRest.Note note -> Note.isTied note
  | NoteOrRest.Rest _ -> false
