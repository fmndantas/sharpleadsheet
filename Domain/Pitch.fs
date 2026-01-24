module Domain.Pitch

type T = { NoteName: NoteName.T; Octave: int }

let create noteName octave = { NoteName = noteName; Octave = octave }

let create3 noteName = create noteName 3

let create4 noteName = create noteName 4

let create5 noteName = create noteName 5

let getNoteName (v: T) = v.NoteName

let getOctave (v: T) = v.Octave
