[<RequireQualifiedAccess>]
module Domain.NoteName

type T =
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

let cicleOfFifths = [ C; G; D; A; E; B; FSharp; CSharp; GSharp; DSharp; ASharp; F ]

let cicleOfFourths = [ C; F; BFlat; EFlat; AFlat; DFlat; GFlat; B; E; A; D; G ]

let semitonesToReachC (n: T) : int =
  match n with
  | C -> 0
  | CSharp
  | DFlat -> 1
  | D -> 2
  | DSharp
  | EFlat -> 3
  | E -> 4
  | F -> 5
  | FSharp -> 6
  | GFlat -> 6
  | G -> 7
  | GSharp -> 8
  | AFlat -> 8
  | A -> 9
  | ASharp -> 10
  | BFlat -> 10
  | B -> 11
