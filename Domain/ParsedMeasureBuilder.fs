module Domain.ParsedMeasureBuilder

open GenericFunctions

open CommonTypes
open ParsedTypes

let aParsedMeasure () : ParsedMeasure = {
  Clef = Clef.G
  TimeSignature = {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  KeySignature = KeySignature NoteName.C
  VoiceEntries = []
}

let withKeySignature (k: KeySignature) (m: ParsedMeasure) : ParsedMeasure = { m with KeySignature = k }

let withTimeSignature (t: TimeSignature) (m: ParsedMeasure) : ParsedMeasure = { m with TimeSignature = t }

let withCNaturalKeySignature (m: ParsedMeasure) : ParsedMeasure =
  NoteName.C |> KeySignature |> flip2 withKeySignature m

let withCommonTimeSignature (m: ParsedMeasure) : ParsedMeasure =
  {
    Numerator = 4
    Denominator = Duration.Quarter
  }
  |> flip2 withTimeSignature m

let withClef (clef: Clef) (m: ParsedMeasure) : ParsedMeasure = { m with Clef = clef }

let withNote (note: Note.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = List.append m.VoiceEntries [ VoiceEntry.fromNote note ]
}

let withRest (rest: Rest.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = List.append m.VoiceEntries [ VoiceEntry.fromRest rest ]
}

let withNotes (notes: Note.T list) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = [ yield! m.VoiceEntries; yield! List.map VoiceEntry.fromNote notes ]
}

let withRepeteadNote (count: int) (note: Note.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = List.append m.VoiceEntries (List.replicate count (VoiceEntry.fromNote note))
}

let withRepeatedRest (count: int) (rest: Rest.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = List.append m.VoiceEntries (List.replicate count (VoiceEntry.fromRest rest))
}

let withVoiceEntry (voiceEntry: VoiceEntry.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = List.append m.VoiceEntries [ voiceEntry ]
}

let withVoiceEntries (voiceEntries: VoiceEntry.T list) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = voiceEntries
}

let withRepeteadSymbols (count: int) (voiceEntry: VoiceEntry.T) (m: ParsedMeasure) : ParsedMeasure = {
  m with
      VoiceEntries = List.append m.VoiceEntries (List.replicate count voiceEntry)
}
