module Domain.Rest

type T = {
  Duration: Duration.T
  Modifiers: Modifier list
  Chord: Chord.T option
}

and Modifier = Text of string

let create (duration: Duration.T) : T = {
  Duration = duration
  Modifiers = []
  Chord = None
}

let getDuration ({ Duration = duration }: T) : Duration.T = duration

let getChord ({ Chord = chord }: T) = chord

let private addModifier (m: Modifier) (rest: T) : T = {
  rest with
      Modifiers = m :: rest.Modifiers
}

let private maybeAddModifier (m: Modifier option) (rest: T) : T =
  m |> Option.map (rest |> flip2 addModifier) |> Option.defaultValue rest

let withChord (chord: Chord.T) (rest: T) : T = { rest with Chord = Some chord }

let maybeWithChord (chord: Chord.T option) (rest: T) : T = { rest with Chord = chord }

let withText (text: string) (rest: T) = addModifier (Text text) rest

let maybeWithText (text: string option) (rest: T) : T =
  maybeAddModifier (Option.map Text text) rest
