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
      Accidental: Accidental
      Octave: int
      Duration: Duration }

type TimeSignature =
    { Numerator: int
      Denominator: Duration }

type KeySignature =
    { NaturalNote: NaturalNote
      Accidental: Accidental }

type MeasureNumber = MeasureNumber of int

[<RequireQualifiedAccess>]
type NoteEvent =
    | Note of Note
    | Pause of Duration

type Measure =
    { MeasureNumber: MeasureNumber
      TimeSignature: TimeSignature
      KeySignature: KeySignature
      Notes: NoteEvent list }

[<RequireQualifiedAccess>]
type Clef =
    | G
    | F

type Part =
    { Name: string
      Clef: Clef
      Measures: Measure list }

type Music = Music of List<Part>

[<RequireQualifiedAccess>]
type MeasureEvent =
    | DefineKeySignature of KeySignature
    | DefineTimeSignature of TimeSignature
    | DefineClef of Clef
