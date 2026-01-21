module Domain.RhythmicNote

type T = { Duration: Duration.T }

let create (duration: Duration.T) = { Duration = duration }

let getDuration (r: T) = r.Duration
