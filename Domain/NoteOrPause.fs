module Domain.NoteOrPause

open Domain.Types

let getDuration (n: NoteOrPause) : Duration =
    match n with
    | NoteOrPause.Note note -> note.Duration
