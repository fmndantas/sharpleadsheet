module Domain.Note

type T = private {
  Pitch: Pitch.T
  Duration: Duration.T
  Modifiers: Modifier list
}

and Modifier = | Tie

/// Create a note without modifiers
let create octave noteName duration = {
  Pitch = Pitch.create noteName octave
  Duration = duration
  Modifiers = []
}

/// Create a note with modifiers
let create' octave modifiers noteName duration = {
  Pitch = Pitch.create noteName octave
  Duration = duration
  Modifiers = modifiers
}

let create2 = create 2

let create4 = create 4

let create4' = create' 4

let createTied4 = create4' [ Tie ]

let create5 = create 5

let create6 = create 6

let getPitch note = note.Pitch

let getDuration note = note.Duration

let isTied note = note.Modifiers |> List.contains Tie
