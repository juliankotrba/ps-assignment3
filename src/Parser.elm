module Parser exposing (..)

import Char

-- Based on Simon Thompson
-- Haskell – The Craft of Functional Programming

type alias Parse a b  = List a -> List (b, List a)

-- Parser primitives

succeed : b -> Parse a b
succeed val inp = [(val, inp)]

fail : Parse a b
fail _ = []

token : a -> Parse a a
token t l = spot ((==) t) l

spot : (a -> Bool) -> Parse a a
spot p l = case l of
  x::xs ->
    if p x then succeed x xs
    else (fail l)
  _ ->  fail l

-- Parser combinators

alt : Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp

infixr 5 >*>
(>*>) : Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp
      =
        let
          res1 = p1 inp
        in
          case res1 of
            (val1, rem1)::_ ->
                let
                  res2 = p2 rem1
                in
                  case res2 of
                    (val2, rem2)::_ -> [((val1, val2), rem2)]
                    _ -> []
            _ -> []

build : Parse a b -> (b -> c) -> Parse a c
build p f inp
        =
          let
            res = p inp
          in
            case res of
              (val, rem)::_ -> [(f val, rem)]
              _ -> []

-- Parser

optionalList : Parse a (List b) -> Parse a (List b)
optionalList p inp
      =
        let
          res = p inp
        in
          case res of
            (val, rem)::_ -> succeed val rem
            _ -> succeed [] inp

number : Parse Char (List Char)
number = (list [] digit)

string : Parse Char (List Char)
string = list [] letter

list : List b -> Parse a b -> Parse a (List b)
list l p inp
    =
      let
        res = p inp
      in
          case res of
            (val, rem1)::_ -> list (l ++ [val]) p rem1
            _ -> succeed l inp

digit : Parse Char Char
digit = spot Char.isDigit

letter : Parse Char Char
letter = spot isLetter

period : Parse Char Char
period = spot ((==)'.')

openingCurlyBrace : Parse Char Char
openingCurlyBrace = spot ((==) '{')

closingCurlyBrace : Parse Char Char
closingCurlyBrace = spot ((==) '}')

isLetter : Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c
