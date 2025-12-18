module Domain.NoteOrRest

open CommonTypes

let getDuration (n: NoteOrRest) : Duration.T =
  match n with
  | NoteOrRest.Note note -> Note.getDuration note
  | NoteOrRest.Rest rest -> Rest.getDuration rest

let isTied (n: NoteOrRest) : bool =
  match n with
  | NoteOrRest.Note note -> Note.isTied note
  | NoteOrRest.Rest _ -> false
