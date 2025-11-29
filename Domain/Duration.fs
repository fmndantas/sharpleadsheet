[<RequireQualifiedAccess>]
module Domain.Duration

type T =
  | Whole
  | WholeDotted
  | Half
  | HalfDotted
  | Quarter
  | QuarterDotted
  | Eighth
  | EighthDotted
  | Sixteenth
  | SixteenthDotted
  | ThirtySecond

type Equivalence = Multiple of int

let getEquivalenceToMinimalDuration =
  function
  | Whole -> Multiple 32
  | WholeDotted -> Multiple 48
  | Half -> Multiple 16
  | HalfDotted -> Multiple 24
  | Quarter -> Multiple 8
  | QuarterDotted -> Multiple 12
  | Eighth -> Multiple 4
  | EighthDotted -> Multiple 6
  | Sixteenth -> Multiple 2
  | SixteenthDotted -> Multiple 3
  | ThirtySecond -> Multiple 1

let private equivalenceToInt e =
  match e with
  | Multiple v -> v

let getEquivalenceBetweenLists (listA: T list) (listB: T list) =
  List.sumBy (getEquivalenceToMinimalDuration >> equivalenceToInt) listA = List.sumBy
    (getEquivalenceToMinimalDuration >> equivalenceToInt)
    listB
