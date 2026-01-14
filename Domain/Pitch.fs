module Domain.Pitch

type T = {
  NoteName: NoteName.T
  Octave: int
}

let create noteName octave = { NoteName = noteName; Octave = octave }

let createMiddle noteName = create noteName 4

let getNoteName (v: T) = v.NoteName

let getOctave (v: T) = v.Octave
