module Domain.ParserStateBuilder

open CommonTypes
open ParsedTypes

let aParserState () : ParserState = {
  CurrentTimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  CurrentKeySignature = KeySignature NoteName.C
  CurrentClef = Clef.G
  CurrentOctave = 4
  LastPitch = None
  LastDuration = None
  LastChord = None
  LastText = None
}

let withCurrentTimeSignature (t: TimeSignature) (s: ParserState) = { s with CurrentTimeSignature = t }

let withCurrentKeySignature (k: KeySignature) (s: ParserState) = { s with CurrentKeySignature = k }

let withCurrentClef (c: Clef) (s: ParserState) = { s with CurrentClef = c }

let withCurrentOctave (o: int) (s: ParserState) = { s with CurrentOctave = o }

let withLastPitch (p: Pitch.T) (s: ParserState) = { s with LastPitch = Some p }

let withLastPitchOption (p: Pitch.T option) (s: ParserState) = { s with LastPitch = p }

let withLastDuration (d: Duration.T) (s: ParserState) = { s with LastDuration = Some d }

let withLastDurationOption (d: Duration.T option) (s: ParserState) = { s with LastDuration = d }

let withLastChord (c: Chord.T) (s: ParserState) = { s with LastChord = Some c }

let withLastChordOption (c: Chord.T option) (s: ParserState) = { s with LastChord = c }

let withoutLastPitch (s: ParserState) = { s with LastPitch = None }

let withoutLastDuration (s: ParserState) = { s with LastDuration = None }

let withoutLastChord (s: ParserState) = { s with LastChord = None }

let withLastText (t: string) (s: ParserState) = { s with LastText = Some t }

let withoutLastText (s: ParserState) = { s with LastText = None }
