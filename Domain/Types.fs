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

[<RequireQualifiedAccess>]
type Clef =
    | G
    | F

type Measure =
    { Clef: Clef
      MeasureNumber: MeasureNumber
      TimeSignature: TimeSignature
      KeySignature: KeySignature
      Notes: NoteOrPause list }

type PartId = PartId of int

type Part =
    { Id: PartId
      Name: string
      Measures: Measure list }

type Music = Music of List<Part>

[<RequireQualifiedAccess>]
type MeasureEvent =
    | DefineKeySignature of KeySignature
    | DefineTimeSignature of TimeSignature
    | DefineClef of Clef
    | NoteOrPause of NoteOrPause

[<RequireQualifiedAccess>]
type Fifth =
    | Zero
    | Flat of int
    | Sharp of int
