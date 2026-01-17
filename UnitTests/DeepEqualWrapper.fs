module UnitTests.DeepEqualWrapper

open DeepEqual.Syntax

let deepEqual (expected: obj) (actual: obj) : unit = actual.ShouldDeepEqual expected
