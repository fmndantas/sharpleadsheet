[<AutoOpen>]
module Domain.GenericFunctions

let flip2<'a, 'b, 'c> (f: 'a -> 'b -> 'c) : 'b -> 'a -> 'c = fun b a -> f a b

let toString<'a> (v: 'a) = v.ToString()
