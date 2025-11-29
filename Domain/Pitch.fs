module Domain.Pitch

type T = private {
  NoteName: NoteName.T
  Octave: int
}

let create noteName octave = { NoteName = noteName; Octave = octave }

let createMiddle noteName = create noteName 4
