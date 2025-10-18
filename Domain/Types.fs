module Domain.Types

[<RequireQualifiedAccess>]
type Duration =
  | Whole
  | WholeDotted
  | Half
  | HalfDotted
  | Quarter
  | QuarterDotted
  | Eighth
  | EighthDotted
  | Sixteenth
  | SixteenthDotted

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

type Pitch = { NoteName: NoteName; Octave: int }

module Note =
  type T = private {
    Pitch: Pitch
    Duration: Duration
  }

  let create noteName octave duration = {
    Pitch = { NoteName = noteName; Octave = octave }
    Duration = duration
  }

  let createMiddle noteName duration = create noteName 4 duration

  let getPitch note = note.Pitch

  let getDuration note = note.Duration

type Rest = Rest of Duration

type TimeSignature = {
  Numerator: int
  Denominator: Duration
}

type KeySignature = KeySignature of NoteName

type MeasureId = MeasureId of int

[<RequireQualifiedAccess>]
type NoteOrRest =
  | Note of Note.T
  | Rest of Rest

[<RequireQualifiedAccess>]
type Clef =
  | G
  | F

type Measure = {
  Clef: Clef
  Id: MeasureId
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  NotesOrRests: NoteOrRest list
}

type PartId = PartId of int

type Part = {
  Id: PartId
  Name: string
  Measures: Measure list
}

type Music = Music of List<Part>

[<RequireQualifiedAccess>]
type MeasureEvent =
  | DefineKeySignature of KeySignature
  | DefineTimeSignature of TimeSignature
  | DefineClef of Clef
  | NoteOrRest of NoteOrRest

[<RequireQualifiedAccess>]
type Fifth =
  | Zero
  | Flat of int
  | Sharp of int
