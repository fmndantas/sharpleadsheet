namespace Domain

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

type NoteAccident =
    | Flat
    | Natural
    | Sharp

type Note =
    { NaturalNote: NaturalNote
      NoteAccident: NoteAccident }

type Octave = Octave of uint

type Pitch = { Note: Note; Octave: Octave }

type NoteEvent =
    { Pitch: Pitch; NoteDuration: Duration }

type ChordEvent =
    { Root: Pitch; ChordDuration: Duration }

type TimeSignature =
    { Numerator: Duration
      Denominator: Duration }

type Measure =
    { TimeSignature: TimeSignature
      NoteEvents: List<NoteEvent>
      ChordEvents: List<ChordEvent> }

type Music = List<Measure>

module XmlConverter =
    ()
