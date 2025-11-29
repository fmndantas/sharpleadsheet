module Domain.ValidatedMeasureBuilder

open Domain.CommonTypes
open Domain.ParsedTypes

// TODO: review usage of this function
let toValidatedMeasure (id: int) (m: ParsedMeasure) : Validated.Measure = { MeasureId = MeasureId id; Parsed = m }
