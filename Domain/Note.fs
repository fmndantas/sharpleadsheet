module Domain.Note

type T = {
  Pitch: Pitch.T
  Duration: Duration.T
  Modifiers: Modifier list
  Chord: Chord.T option
}

and Modifier = | Tie

/// Create a note without modifiers
let create octave noteName duration = {
  Pitch = Pitch.create noteName octave
  Duration = duration
  Modifiers = []
  Chord = None
}

/// Create a note with modifiers
let create' octave modifiers noteName duration = {
  Pitch = Pitch.create noteName octave
  Duration = duration
  Modifiers = modifiers
  Chord = None
}

let create2 = create 2

let create4 = create 4

let create4' = create' 4

let createTied4 = create4' [ Tie ]

let create5 = create 5

let create6 = create 6

let getPitch note = note.Pitch

let getDuration note = note.Duration

let getChord note = note.Chord

let isTied note = note.Modifiers |> List.contains Tie

let withChord (chord: Chord.T) (note: T) = { note with Chord = Some chord }

let maybeWithChord (chord: Chord.T option) (note: T) = { note with Chord = chord }
