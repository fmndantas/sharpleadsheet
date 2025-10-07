module Domain.KeySignature

open Domain.Types

type T = KeySignature

let fifths (KeySignature n: T) : Fifth =
  let equalToNoteName = (=) (NoteName.semitonesToReachC n)

  let numberOfSharps =
    NoteName.cicleOfFifths
    |> List.indexed
    |> List.find (snd >> NoteName.semitonesToReachC >> equalToNoteName)
    |> fst

  let numberOfFlats =
    NoteName.cicleOfFourths
    |> List.indexed
    |> List.find (snd >> NoteName.semitonesToReachC >> equalToNoteName)
    |> fst

  if min numberOfFlats numberOfSharps = 0 then
    Fifth.Zero
  elif numberOfSharps < numberOfFlats then
    Fifth.Sharp numberOfSharps
  else
    Fifth.Flat numberOfFlats
