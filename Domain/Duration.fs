module Domain.Duration

open Domain.Types

let private durations =
    [ Duration.WholeNote
      Duration.HalfNote
      Duration.QuarterNote
      Duration.EightNote
      Duration.SixteenthNote ]

let private minimalDuration = List.last durations

let getEquivalence (unitOfEquivalence: Duration) (targetDuration: Duration) : DurationEquivalence =
    let u = durations |> List.findIndex ((=) unitOfEquivalence) |> pown 2
    let t = durations |> List.findIndex ((=) targetDuration) |> pown 2

    if u >= t then
        u / t |> DurationEquivalence.Multiple
    else
        t / u |> DurationEquivalence.Divider

let getEquivalenceToMinimalDuration = getEquivalence minimalDuration
