module Domain.Types

[<RequireQualifiedAccess>]
type Duration =
    | WholeNote
    | HalfNote
    | QuarterNote
    | EightNote
    | SixteenthNote

[<RequireQualifiedAccess>]
type NaturalNote =
    | C
    | D
    | E
    | F
    | G
    | A
    | B

[<RequireQualifiedAccess>]
type Accidental =
    | Flat
    | Natural
    | Sharp

type Note =
    { NaturalNote: NaturalNote
      Accidental: Accidental }

type Octave = Octave of int

type Pitch = { Note: Note; Octave: Octave }

type NoteEvent =
    { Pitch: Pitch; NoteDuration: Duration }

type ChordEvent =
    { Root: Pitch; ChordDuration: Duration }

type TimeSignature =
    { Numerator: int
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

[<RequireQualifiedAccess>]
type MeasureEvent =
    | DefineKeySignature of KeySignature
    | DefineTimeSignature of TimeSignature
