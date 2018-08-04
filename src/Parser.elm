module Parser exposing (..)

import Char

-- Based on Simon Thompson
-- Haskell – The Craft of Functional Programming

type alias Parse a b  = List a -> List (b, List a)

-- Parser primitives

succeed : b -> Parse a b
succeed val inp = [(val, inp)]

none : Parse a b
none _ = []

token : a -> Parse a a
token t l = spot (\x -> x==t) l

spot : (a -> Bool) -> Parse a a
spot p l = case l of
  x::xs ->
    if p x then succeed x xs
    else (none l)
  _ ->  none l

-- Parser combinators

alt : Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp

-- TODO (>*>) : Parse a b -> Parse a c -> Parse a (b,c)
--              infixr 9 >*>

infixr 9 >*>
seq : Parse a b -> Parse a c -> Parse a (b,c)
seq p1 p2 inp
        =
          let
            res1 = p1 inp
          in
            case res1 of
              (val1, rem1)::_ ->  let
                                    res2 = p2 rem1
                                  in
                                    case res2 of
                                      (val2, rem2)::_ -> [((val1, val2), rem2)]
                                      _ -> []

              _ -> []

-- Parser

digit : Parse Char Char
digit = spot Char.isDigit

letter : Parse Char Char
letter = spot isLetter

openCurlyBrace : Parse Char Char
openCurlyBrace = spot ((==) '{')

closingCurlyBrace : Parse Char Char
closingCurlyBrace = spot ((==) '}')

isLetter : Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c
