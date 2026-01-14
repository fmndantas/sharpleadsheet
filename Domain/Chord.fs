module Domain.Chord

type T = {
  Root: NoteName.T
  Bass: NoteName.T option
  Kind: string option
}

let create (root: NoteName.T) (bass: NoteName.T option) (kind: string option) : T = {
  Root = root
  Bass = bass
  Kind = kind
}

let createWithRoot (root: NoteName.T) : T = create root None None

let createWithKind (root: NoteName.T) (kind: string) : T = create root None (Some kind)

let createWithBass (root: NoteName.T) (bass: NoteName.T) : T = create root (Some bass) None

let createWithBassAndKind (root: NoteName.T) (bass: NoteName.T) (kind: string) : T = create root (Some bass) (Some kind)

let getRoot ({ Root = root }: T) = root

let getKind ({ Kind = kind }: T) = kind

let getBass ({ Bass = bass }: T) = bass
