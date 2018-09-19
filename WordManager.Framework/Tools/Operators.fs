namespace WordManager.Framework.Tools

module Operators =

    let (?=) (q: bool) (yes: 'a, no: 'a) = if q then yes else no

    let inline (|?) (a: 'a option) b = if a.IsSome then a.Value else b
