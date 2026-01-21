module Domain.VoiceEntry

type T = { Kind: Kind; Modifiers: Modifier list }

and Kind =
  | Note of Note.T
  | Rest of Rest.T
  | RhythmicNote of RhythmicNote.T

and Modifier =
  | Tie
  | Chord of Chord.T
  | Text of string

let private addModifier (m: Modifier) (n: T) = { n with Modifiers = m :: n.Modifiers }

let withTie: T -> T = addModifier Tie

let withChord (chord: Chord.T) (n: T) : T = addModifier (Chord chord) n

let withChordOption (chord: Chord.T option) (n: T) : T =
  chord |> Option.map (n |> (withChord |> flip2)) |> Option.defaultValue n

let withText (text: string) (n: T) : T = addModifier (Text text) n

let withTextOption (text: string option) (n: T) : T =
  text |> Option.map (n |> (withText |> flip2)) |> Option.defaultValue n

let fromNote (n: Note.T) : T = { Kind = Note n; Modifiers = [] }

let fromNoteWithTie: Note.T -> T = fromNote >> withTie

let fromRest (r: Rest.T) : T = { Kind = Rest r; Modifiers = [] }

let fromRhythmicNote (r: RhythmicNote.T) : T = {
  Kind = RhythmicNote r
  Modifiers = []
}

let isTied (n: T) : bool =
  n.Modifiers
  |> List.exists (function
    | Tie when n.Kind.IsNote -> true
    | _ -> false)

let getDuration (n: T) : Duration.T =
  match n.Kind with
  | Note note -> Note.getDuration note
  | Rest rest -> Rest.getDuration rest

let getChord (n: T) : Chord.T option =
  n.Modifiers
  |> List.tryFind _.IsChord
  |> Option.bind (function
    | Chord v -> Some v
    | _ -> None)

let getText (n: T) : string option =
  n.Modifiers
  |> List.tryFind _.IsText
  |> Option.bind (function
    | Text v -> Some v
    | _ -> None)

let fold (noteF: Note.T -> 'a) (restF: Rest.T -> 'a) (n: T) : 'a =
  match n.Kind with
  | Note n -> noteF n
  | Rest r -> restF r
