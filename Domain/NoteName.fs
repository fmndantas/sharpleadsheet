module Domain.NoteName

open Domain.Types

let cicleOfFifths = [
  NoteName.C
  NoteName.G
  NoteName.D
  NoteName.A
  NoteName.E
  NoteName.B
  NoteName.FSharp
  NoteName.CSharp
  NoteName.GSharp
  NoteName.DSharp
  NoteName.ASharp
  NoteName.F
]

let cicleOfFourths = [
  NoteName.C
  NoteName.F
  NoteName.BFlat
  NoteName.EFlat
  NoteName.AFlat
  NoteName.DFlat
  NoteName.GFlat
  NoteName.B
  NoteName.E
  NoteName.A
  NoteName.D
  NoteName.G
]

let semitonesToReachC (n: NoteName) : int =
  match n with
  | NoteName.C -> 0
  | NoteName.CSharp
  | NoteName.DFlat -> 1
  | NoteName.D -> 2
  | NoteName.DSharp
  | NoteName.EFlat -> 3
  | NoteName.E -> 4
  | NoteName.F -> 5
  | NoteName.FSharp -> 6
  | NoteName.GFlat -> 6
  | NoteName.G -> 7
  | NoteName.GSharp -> 8
  | NoteName.AFlat -> 8
  | NoteName.A -> 9
  | NoteName.ASharp -> 10
  | NoteName.BFlat -> 10
  | NoteName.B -> 11
