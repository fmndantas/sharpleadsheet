module Domain.Rest

type T = private {
  Duration: Duration.T
  Chord: Chord.T option
}

let create (duration: Duration.T) : T = { Duration = duration; Chord = None }

let getDuration ({ Duration = duration }: T) : Duration.T = duration

let getChord ({ Chord = chord }: T) = chord

let withChord (chord: Chord.T) (rest: T) : T = { rest with Chord = Some chord }

let maybeWithChord (chord: Chord.T option) (rest: T) : T = { rest with Chord = chord }
