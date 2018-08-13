module Parser exposing (..)

import Char

-- Based on Simon Thompson
-- Haskell – The Craft of Functional Programming

type alias Parse a b  = List a -> Result String (List (b, List a))

-- Parser primitives

succeed : b -> Parse a b
succeed val inp = Ok [(val, inp)]

fail : String -> Parse a b
fail errMsg _ = Err errMsg

symbol : a -> Parse a a
symbol t l = spot (toString t) ((==) t) l

spot : String -> (a -> Bool) -> Parse a a
spot desc p l =
  case l of
    x::xs ->
      if p x then succeed x xs
        else (fail desc l)
    _ ->  fail desc l

-- Parser combinators

alt : Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp =
  let
    res1 = p1 inp
    res2 = p2 inp
  in
    case (res1,res2) of
      (Err e1, Err e2) -> Err <| e1 ++ " || " ++ e2
      (Ok r1, Ok r2) -> Ok (r1++r2)
      (Ok r1, _) -> Ok r1
      (_, Ok r2) -> Ok r2

infixr 5 >*>
(>*>) : Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp=
  let
    res1 = p1 inp
  in
    case res1 of
      Ok ((val1, rem1)::_) ->
        let
          res2 = p2 rem1
        in
          case res2 of
            Ok ((val2, rem2)::_) -> Ok [((val1, val2), rem2)]
            Ok [] -> Ok []
            Err errMsg -> fail errMsg []
      Err errMsg -> fail errMsg []
      Ok [] -> Ok []

build : Parse a b -> (b -> c) -> Parse a c
build p f inp =
  let
    res = p inp
  in
    case res of
      Ok ((val, rem)::_) -> Ok [(f val, rem)]
      Ok [] -> Ok []
      Err errMsg -> fail errMsg []

-- Parser

option : Parse a (List b) -> Parse a (List b)
option p inp =
  let
    res = p inp
  in
    case res of
      Ok ((val, rem)::_) -> succeed val rem
      _ -> succeed [] inp

many0 : List b -> Parse a b -> Parse a (List b)
many0 l p inp =
  let
    res = p inp
  in
    case res of
      Ok ((val, rem1)::_) -> list (List.append l [val]) p rem1
      _ -> succeed l inp

number : Parse Char (List Char)
number = (list [] digit)

string : Parse Char (List Char)
string = list [] letter

list : List b -> Parse a b -> Parse a (List b)
list l p inp =
  let
    res = p inp
  in
    case res of
      Ok ((val, rem1)::_) -> list (l ++ [val]) p rem1
      Ok [] -> succeed l inp
      Err errMsg -> if (List.isEmpty l) then fail ("Expecting list of " ++ errMsg) [] else succeed l inp

digit : Parse Char Char
digit = spot "([0-9])" Char.isDigit

letter : Parse Char Char
letter = spot "([a-zA-Z])" isLetter

period : Parse Char Char
period = symbol '.'

leftParenthesis : Parse Char Char
leftParenthesis = symbol '('

rightParenthesis : Parse Char Char
rightParenthesis = symbol ')'

asterisk : Parse Char Char
asterisk = symbol '*'

minus : Parse Char Char
minus = symbol '-'

plus : Parse Char Char
plus = symbol '+'

colon : Parse Char Char
colon = symbol ':'

space : Parse Char Char
space = symbol ' '

openingCurlyBrace : Parse Char Char
openingCurlyBrace = symbol '{'

closingCurlyBrace : Parse Char Char
closingCurlyBrace = symbol '}'

isLetter : Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c

{- Language specific parser

Syntax of the language:

<program> ::= { <rule> }
    <rule> ::= <head> <body> ’.’
    <head> ::= <atom>
    <body> ::= { ’:’ <goal> }
    <goal> ::= <atom>
        | <pattern> ’=’ <pattern>
        | ’$’ <pattern> <pattern> ’-’ <pattern> <pattern>
    <atom> ::= <name> { <pattern> } ’-’ { <pattern> }
    <name> ::= <token>
    <pattern> ::= ’(’ { [ ’+’ ] <token> } [ ’*’ <token> ] ’)’
-}

name = token

pattern =
  build ((build (build (leftParenthesis >*> many0TokenWithOptionalPlus) (\(res1,res2)->[res1]::res2)
    >*>
      optionalTokenWith) (\(res1,res2)-> if (List.isEmpty res2) then res1 else res1++[res2]))
        >*> rightParenthesis) (\(res1,res2)-> res1++[[res2]])

token = string

many0TokenWithOptionalPlus = many0 [] (build((option (build plus (\c->c::[]))) >*> token) (\(r1,r2)->r1++r2))

optionalTokenWith = option (build (asterisk >*> token) (\(res1,res2)->res1::res2))
