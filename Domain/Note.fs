module Domain.Note

type T = { Pitch: Pitch.T; Duration: Duration.T }

let create (octave: int) (noteName: NoteName.T) (duration: Duration.T) : T = {
  Pitch = Pitch.create noteName octave
  Duration = duration
}

let create2 = create 2

let create4 = create 4

let create5 = create 5

let create6 = create 6

let getPitch (note: T) : Pitch.T = note.Pitch

let getDuration (note: T) : Duration.T = note.Duration
