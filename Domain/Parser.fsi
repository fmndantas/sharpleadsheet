module Domain.Parser

open FParsec

open ParsedTypes

open CommonTypes

module Functions =
    val pclef : Parser<Clef, ParserState>
    val pNoteName : Parser<NoteName.T, ParserState>
