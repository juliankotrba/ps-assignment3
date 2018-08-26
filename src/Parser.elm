module Parser exposing (..)

import Char

-- Based on Simon Thompson
-- Haskell – The Craft of Functional Programming

{-| Parser definition
The fist parameter is the input to be parsed. Most of the cases it will be a (List Char).
The parser result consists of two parts:
    Err (errorMessage, Maybe alreadyParsed, unparsedInput)
    Ok List (parsed, unparsedInput)
 -}
type alias Parse a b  = List a -> Result (String, Maybe b, List a) (List (b, List a))

-- Parser primitives

succeed : b -> Parse a b
succeed val inp = Ok [(val, inp)]

fail : String -> b -> Parse a b
fail errMsg p np = Err (errMsg, Just p, np)

symbol : a -> Parse a a
symbol t l = spot ("Expecting: " ++ toString t) ((==) t) l

spot : String -> (a -> Bool) -> Parse a a
spot desc p l =
  case l of
    x::xs ->
      if p x then succeed x xs
        else (Err (desc, Nothing, l))
    _ ->  Err (desc, Nothing, l)--fail desc l

-- Parser combinators
-- test : goal <| String.toList "$()()-()("

alt : Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp =
  let
    res1 = p1 inp
    res2 = p2 inp
  in
    case (res1,res2) of
      (Err (e1, p1, np1), Err (e2, p2, np2)) ->
        Err <| (((appendExpecting e1) ++ " || " ++ (appendExpecting e2)), p2, np2) -- TODO

      (Ok r1, Ok r2) -> Ok (r1++r2)
      (Ok r1, _) -> Ok r1
      (_, Ok r2) -> Ok r2

infixr 5 >*>
(>*>) : Parse a b -> Parse a c -> Parse a (Maybe b, Maybe c)
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
            Ok ((val2, rem2)::_) -> Ok [((Just val1, Just val2), rem2)]
            Ok [] -> Ok []
            Err (errMsg, p, np) -> Err (appendExpecting errMsg, Just (Just val1, p), np)
      Ok [] -> Ok []
      Err (errMsg, p, np) -> Err (appendExpecting errMsg, if p == Nothing then Nothing else Just (p, Nothing), np)


build : Parse a b -> (b -> c) -> Parse a c
build parser f inp =
  let
    res = parser inp
  in
    case res of
      Ok ((val, rem)::_) -> Ok [(f val, rem)]
      Ok [] -> Ok []
      Err (errMsg, p, np) -> Err (appendExpecting errMsg, Maybe.map f p, np)

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

many1 : List b -> Parse a b -> Parse a (List b)
many1 l parser inp =
  let
    res = parser inp
  in
    case res of
      Ok ((val, rem1)::_) -> list (List.append l [val]) parser rem1
      Ok [] -> succeed l inp
      Err (errMsg, p, np) -> Err ((appendExpecting errMsg), unwrapToListMaybe p, inp)

unwrapToListMaybe : Maybe a -> Maybe (List a)
unwrapToListMaybe ma =
  case ma of
    Just v -> Just [v]
    _ -> Just []

number : Parse Char (List Char)
number = (list [] digit)

string : Parse Char (List Char)
string = list [] letter

stringWithSpaces : Parse Char (List Char)
stringWithSpaces = list [] (alt digit (alt letter (symbol ' ')))

list : List b -> Parse a b -> Parse a (List b)
list l parser inp =
  let
    res = parser inp
  in
    case res of
      Ok ((val, rem1)::_) -> list (l ++ [val]) parser rem1
      Ok [] -> succeed l inp
      Err (errMsg, p, np) -> if (List.isEmpty l) then Err (("Expecting list of " ++ errMsg), Nothing, inp) else succeed l inp

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

equals : Parse Char Char
equals = symbol '='

dollar : Parse Char Char
dollar = symbol '$'

space : Parse Char Char
space = symbol ' '

openingCurlyBrace : Parse Char Char
openingCurlyBrace = symbol '{'

closingCurlyBrace : Parse Char Char
closingCurlyBrace = symbol '}'

isLetter : Char -> Bool
isLetter c = Char.isUpper c || Char.isLower c

isSpecialSymbol c
   = c == ':'
  || c == '$'
  || c == '.'
  || c == '='
  || c == '-'
  || c == '('
  || c == ')'
  || c == '+'
  || c == '*'

isEscapeSymbol c = c == '\\'

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

type SyntaxComponent
  = Default String
  | Variable String
  | Literal String
  | Name String
  | Symbol String
  | Marked String
  | Unparsed String
  | Error String
  | Linebreak

buildVar p = build p (\res -> Variable <| String.fromList res)
buildLiteral p = build p (\res -> Literal <| String.fromList res)
buildDefault p = build p (\res -> Default <| String.fromList res)
buildName p = build p (\res -> Name <| String.fromList res)
buildSymbol p = build p (\res -> Symbol <| String.fromList res)

rule =
  --> <head>
  head
  |>*>|
  --> <body>
  body
  |>*>|
  --> ’.’
  (wrapInList (buildSymbol (wrapInList period)))

head = atom

body =
  build (many0 [] (
    --> ’:’
    (wrapInList((buildSymbol (wrapInList colon))))
    |>*>|
    --> <goal>
    goal)) (\c-> List.concat c)

goal =
  alt goalFirstBranch
  <|
  alt goalSecondBranch goalThirdBranch

goalFirstBranch = atom

goalSecondBranch =
  --> <pattern>
  pattern
  |>*>|
  --> ’=’
  (wrapInList (buildSymbol (wrapInList  equals)))
  |>*>|
  --> <pattern>
  pattern

goalThirdBranch =
  -->  ’$’
  (wrapInList (buildSymbol (wrapInList  dollar)))
  |>*>|
  --> <pattern> <pattern>
  twoPattern
  |>*>|
  --> ’-’
  (wrapInList (buildSymbol (wrapInList  minus)))
  |>*>|
  --> <pattern> <pattern>
  twoPattern

twoPattern = pattern |>*>| pattern

name = string

atom =
  --> name
  build ((wrapInList (wrapInList (buildName (name))))
  |>*>|
  --> { <pattern> }
  many0Pattern
  |>*>|
  --> ’-’
  (wrapInList (wrapInList (buildDefault (wrapInList minus))))
  |>*>|
  -- { <pattern> }
  many0Pattern) (\c -> List.foldr (++) [] c)

pattern =
  (build(
  --> ’(’ { [ ’+’ ] <token> }
  (((wrapInList (buildDefault(wrapInList leftParenthesis))) |>*>| many0PlusTokenOrLiteral))
  >*>
  --> [ ’*’ <token> ]
  optionalTokenWithAsterisk) (\(res1,res2) -> (Maybe.withDefault [(Error "")] res1)++[(Maybe.withDefault (Error "") res2)]))
  |>*>|
  --> ’)’
  wrapInList (buildDefault (wrapInList (rightParenthesis)))

token =  build (many1 [] (alt string specialSymbolParser)) (\res -> List.foldr (++) [] res)

literal =
  build
   (many1 [] (alt stringWithSpaces specialSymbolParser))
   (\res -> List.foldr (++) [] res)

specialSymbolParser =
  build
  (spot ":, ., =, $, -, (, +, *, ) prefixed with \\" isEscapeSymbol
    >*>
      (spot ":, ., =, $, -, (, +, *, ) prefixed with \\" isSpecialSymbol))
  (\(res1,res2)->(Maybe.withDefault ' ' res1)::(Maybe.withDefault ' ' res2)::[])

many0Pattern = many0 [] pattern

many0PlusTokenOrLiteral =
  many0 [] (alt (buildLiteral literal) (buildVar (option (wrapInList (plus)) |>*>| token)))

optionalTokenWithAsterisk =
  buildVar (option <| (((wrapInList asterisk) |>*>| stringWithSpaces)))

-- Helpers

appendExpecting : String -> String
appendExpecting s =
  if (String.contains "Expecting" s) then s else ("Expecting: " ++ s)

wrapInList p = build p (\c->[c])

infixr 5 |>*>|
(|>*>|) p1 p2 = build (p1 >*> p2) (\(res1,res2)-> (Maybe.withDefault [] res1) ++ (Maybe.withDefault [] res2))
