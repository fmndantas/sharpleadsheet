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

module Pitch =
  type T = private {
    NoteName: NoteName
    Octave: int
  }

  let create noteName octave = { NoteName = noteName; Octave = octave }

  let createMiddle noteName = create noteName 4

module Note =
  type T = private {
    Pitch: Pitch.T
    Duration: Duration
    Modifiers: Modifier list
  }

  and Modifier = | Tie

  let create octave noteName duration = {
    Pitch = Pitch.create noteName octave
    Duration = duration
    Modifiers = []
  }

  let create' octave modifiers noteName duration = {
    Pitch = Pitch.create noteName octave
    Duration = duration
    Modifiers = modifiers
  }

  let create2 = create 2

  let create4 = create 4

  let create4' = create' 4

  let createTied4 = create4' [ Tie ]

  let create5 = create 5

  let create6 = create 6

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

type UnvalidatedMeasure = {
  Id: MeasureId
  TimeSignature: TimeSignature
  KeySignature: KeySignature
  Clef: Clef
  NotesOrRests: NoteOrRest list
}

type PartId = PartId of int

type UnvalidatedPart = {
  Id: PartId
  Name: string
  Measures: UnvalidatedMeasure list
}

type Music = Unvalidated of List<UnvalidatedPart>

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
