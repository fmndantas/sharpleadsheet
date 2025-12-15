module Domain.ValidatedMeasureBuilder

open CommonTypes
open ParsedTypes

// TODO: review usage of this function
let toValidatedMeasure (id: int) (m: ParsedMeasure) : Validated.Measure = { MeasureId = MeasureId id; Parsed = m }
