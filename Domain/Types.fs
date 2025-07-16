module Domain.Types

[<RequireQualifiedAccess>]
type Duration =
    | WholeNote
    | HalfNote
    | QuarterNote
    | EightNote
    | SixteenthNote

[<RequireQualifiedAccess>]
type DurationEquivalence = 
    | Multiple of int
    | Divider of int

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

[<RequireQualifiedAccess>]
type NoteName =
    | C
    | CSharp
    | DFlat
    | D
    | DSharp
    | EFlat
    | E
    | F
    | FSharp
    | GFlat
    | G
    | GSharp
    | AFlat
    | A
    | ASharp
    | BFlat
    | B

type Note =
    { NoteName: NoteName
      Octave: int
      Duration: Duration }

type TimeSignature =
    { Numerator: int
      Denominator: Duration }

type KeySignature = KeySignature of NoteName

type MeasureNumber = MeasureNumber of int

[<RequireQualifiedAccess>]
type NoteOrPause = Note of Note

type Measure =
    { MeasureNumber: MeasureNumber
      TimeSignature: TimeSignature
      KeySignature: KeySignature
      Notes: NoteOrPause list }

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
    | Note of Note

[<RequireQualifiedAccess>]
type Fifth =
    | Zero
    | Flat of int
    | Sharp of int
