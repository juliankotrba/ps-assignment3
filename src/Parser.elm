module Parser exposing (..)

-- Based on Simon Thompson
-- Haskell – The Craft of Functional Programming

type alias Parse a b  = List a -> List (b, List a)

-- Primitives

succeed : b -> Parse a b
succeed val inp = [(val, inp)]

none : Parse a b
none _ = []

-- Parser combinators

token : a -> Parse a a
token t l = spot (\x -> x==t) l

spot : (a -> Bool) -> Parse a a
spot p l = case l of
  x::xs ->
    if p x then [(x, xs)]
    else []
  _ ->  []
