module Parser exposing (..)

import Char

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
    if p x then succeed x xs
    else (none l)
  _ ->  none l

alt : Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp

digit : Parse Char Char
digit = spot Char.isDigit

letter = spot isLetter

isLetter : Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c
