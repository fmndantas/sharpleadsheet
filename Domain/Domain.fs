module Domain.Types

type Duration =
    | WholeNote
    | HalfNote
    | QuarterNote
    | EightNote
    | SixteenthNote

type NaturalNote =
    | C
    | D
    | E
    | F
    | G
    | A
    | B

type Accidental =
    | Flat
    | Natural
    | Sharp

type Note =
    { NaturalNote: NaturalNote
      Accidental: Accidental }

type Octave = Octave of uint

type Pitch = { Note: Note; Octave: Octave }

type NoteEvent =
    { Pitch: Pitch; NoteDuration: Duration }

type ChordEvent =
    { Root: Pitch; ChordDuration: Duration }

type TimeSignature =
    { Numerator: uint
      Denominator: Duration }

type KeySignature = KeySignature of Note

type MeasureNumber = MeasureNumber of int

type Measure =
    { MeasureNumber: MeasureNumber
      TimeSignature: TimeSignature
      KeySignature: KeySignature
      NoteEvents: NoteEvent list
      ChordEvents: ChordEvent list }

type Part =
    { Name: string; Measures: Measure list }

type Music = Music of List<Part>
