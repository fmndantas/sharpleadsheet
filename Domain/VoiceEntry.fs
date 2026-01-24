module Domain.VoiceEntry

type T = { Kind: Kind; Modifiers: Modifiers }

and Kind =
  | Note of Note.T
  | Rest of Rest.T
  | RhythmicNote of RhythmicNote.T

and Modifiers = {
  IsTied: bool
  Chord: Chord.T option
  Text: string option
}

let private defaultModifiers = {
  IsTied = false
  Chord = None
  Text = None
}

let private updateModifiers (f: Modifiers -> Modifiers) (v: T) = { v with Modifiers = f v.Modifiers }

let withTie: T -> T = updateModifiers (fun m -> { m with IsTied = true })

let withChord (chord: Chord.T) (v: T) : T =
  updateModifiers (fun m -> { m with Chord = Some chord }) v

let withChordOption (chord: Chord.T option) (n: T) : T =
  chord |> Option.map (n |> (withChord |> flip2)) |> Option.defaultValue n

let withText (text: string) (v: T) : T =
  updateModifiers (fun m -> { m with Text = Some text }) v

let withTextOption (text: string option) (n: T) : T =
  text |> Option.map (n |> (withText |> flip2)) |> Option.defaultValue n

let fromNote (n: Note.T) : T = {
  Kind = Note n
  Modifiers = defaultModifiers
}

let fromNoteWithTie: Note.T -> T = fromNote >> withTie

let fromRest (r: Rest.T) : T = {
  Kind = Rest r
  Modifiers = defaultModifiers
}

let fromRhythmicNote (r: RhythmicNote.T) : T = {
  Kind = RhythmicNote r
  Modifiers = defaultModifiers
}

let isTied (n: T) : bool = not n.Kind.IsRest && n.Modifiers.IsTied

let getPitch (n: T) : Pitch.T option =
  match n.Kind with
  | Note note -> Some(Note.getPitch note)
  | _ -> None

let getDuration (n: T) : Duration.T =
  match n.Kind with
  | Note note -> Note.getDuration note
  | Rest rest -> Rest.getDuration rest
  | RhythmicNote rhythmicNote -> RhythmicNote.getDuration rhythmicNote

let getChord (n: T) : Chord.T option = n.Modifiers.Chord

let getText (n: T) : string option = n.Modifiers.Text

let fold (noteF: Note.T -> 'a) (restF: Rest.T -> 'a) (rhythmicNoteF: RhythmicNote.T -> 'a) (n: T) : 'a =
  match n.Kind with
  | Note n -> noteF n
  | Rest r -> restF r
  | RhythmicNote r -> rhythmicNoteF r

let isNote (n: T) : bool =
  n |> fold (fun _ -> true) (fun _ -> false) (fun _ -> false)
