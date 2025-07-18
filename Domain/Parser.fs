module Domain.Parser

open Domain.Types
open Domain.MeasureBuilder

let parse (fileContent: string) : Music =
    Music
        [ { Name = "Piano"
            Measures =
              [ aMeasure 1
                |> withCNaturalKeySignature
                |> withTimeSignature
                    { Numerator = 2
                      Denominator = Duration.HalfNote }
                |> withNote
                    { NoteName = NoteName.C
                      Octave = 4
                      Duration = Duration.EightNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.EightNote }
                |> withNote
                    { NoteName = NoteName.E
                      Octave = 4
                      Duration = Duration.EightNote }
                |> withNote
                    { NoteName = NoteName.D
                      Octave = 4
                      Duration = Duration.EightNote }

                aMeasure 2
                |> withCNaturalKeySignature
                |> withTimeSignature
                    { Numerator = 2
                      Denominator = Duration.HalfNote } ] } ]
