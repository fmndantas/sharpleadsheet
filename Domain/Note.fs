module Domain.Note

open Domain.Types

// TEST: Note.generateEvents
let generateEvents (m: Measure) : NoteEvent list = m.Notes
