module Domain.Chord

type T = private {
  Root: NoteName.T
  Bass: NoteName.T option
  Kind: string option
}

let createWithBassAndKind (root: NoteName.T) (bass: NoteName.T) (kind: string) : T = {
  Root = root
  Bass = Some bass
  Kind = Some kind
}
