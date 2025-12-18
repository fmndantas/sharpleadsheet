module Domain.Rest

type T = private { Duration: Duration.T }

let create (duration: Duration.T) = { Duration = duration }

let getDuration ({ Duration = duration }: T) = duration
