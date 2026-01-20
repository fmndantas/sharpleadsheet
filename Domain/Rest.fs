module Domain.Rest

type T = { Duration: Duration.T }

let create (duration: Duration.T) : T = { Duration = duration }

let getDuration (rest: T) : Duration.T = rest.Duration
