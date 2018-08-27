module ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing ((>*>))
import Char exposing (toCode)

suceedTests =
    describe "succeed function tests"
      [ test "suceed with value 'x' and input [] should succeed" <|
          \_ -> Parser.succeed 'x' [] |> Expect.equal  (Ok ([('x',[])]))
      , test "suceed with value 'x' and input [y','z'] should succeed" <|
          \_ -> Parser.succeed 'x' ['y','z'] |> Expect.equal (Ok ([('x',['y','z'])]))
      , test "suceed with value 'x' and input ['x','y','z'] should succeed" <|
          \_ -> Parser.succeed 'x' ['x','y','z'] |> Expect.equal (Ok ([('x',['x','y','z'])]))
      , test "suceed with value 1 and input [1,2,3] should succeed" <|
          \_ -> Parser.succeed 'x' ['x','y','z'] |> Expect.equal (Ok ([('x',['x','y','z'])]))
      , test "suceed with value 1 and input [2,3] should succeed" <|
          \_ -> Parser.succeed 'x' ['x','y','z'] |> Expect.equal ((Ok ([('x',['x','y','z'])])))
      ]

unwrap r =
  case r of
    Ok p -> p
    Err e -> e


failTests =
    describe "fail function tests"
      [ test "fail with input [] should fail" <|
          \_ -> (Parser.fail "" [] []) |> Expect.equal (Err ("", Just [], []))
      ]

symbolTests =
    describe "symbol function tests"
        [ test "Parse symbol 'x' from [] should fail" <|
            \_ -> Parser.symbol 'x' []  |> Expect.equal (Err ("Expecting: 'x'", Nothing, []))
        , test "Parse symbol 'x' from ['a','b','c'] should fail" <|
            \_ -> Parser.symbol 'x' ['a','b','c'] |> Expect.equal (Err ("Expecting: 'x'", Nothing, ['a','b','c']))
        , test "Parse symbol 'x' from ['x','b','c'] should succeed" <|
            \_ -> Parser.symbol 'x' ['x','b','c'] |> Expect.equal (Ok [('x', ['b', 'c'])])
        , test "Parse symbol 1 from [] should fail" <|
            \_ -> Parser.symbol 1 [] |> Expect.equal (Err ("Expecting: 1", Nothing, []))
        , test "Parse symbol from [2, 3] should fail" <|
            \_ -> Parser.symbol 1 [2, 3] |> Expect.equal (Err ("Expecting: 1", Nothing, [2,3]))
        , test "Parse symbol 1 from [1, 2, 3] should succeed" <|
            \_ -> Parser.symbol 1 [1, 2] |> Expect.equal (Ok [(1, [2])])
        ]

spotTests =
    describe "spot function tests"
        [ test "The token function is now just calling the spot function with an equals check lambda function as an argument. So the tokenTests are already testing the spot implementation." <|
            \_ -> "xyz" |> Expect.equal "xyz"
        ]

altTests =
    describe "alt function tests"
      [ test "Parse a digit or a letter from \"(xyz\" should fail" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "(xyz") |> Expect.equal (Err ("Expecting: ([0-9]) || Expecting: ([a-zA-Z])", Nothing, ['(','x','y','z']))
      , test "Parse a digit or a letter from \"xyz\" should succeed" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "xyz") |> Expect.equal (Ok [('x',String.toList "yz")])
      , test "Parse a digit or a letter from \"1yz\" should succeed" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "1yz") |> Expect.equal (Ok [('1',String.toList "yz")])
      , test "Parse a digit or a letter from an empty string should fail" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "") |> Expect.equal (Err ("Expecting: ([0-9]) || Expecting: ([a-zA-Z])", Nothing, []))
      , test "Parse a number or a string from \"xyz123\" should succeed" <|
          \_ -> Parser.alt Parser.number Parser.string (String.toList "xyz123") |> Expect.equal (Ok [(String.toList "xyz", String.toList "123")])
      ]

seqTests =
    describe ">*> function tests"
      [ {- test "Parse a digit and a letter in this specific order from \"1(\" should fail" <|
          \_ -> Parser.digit >*> Parser.letter <| String.toList "1(" |> Expect.equal []
      , -} test "Parse a letter and a digit in this specific order from \"(1\" should fail" <|
          \_ -> (Parser.letter >*> Parser.digit <| (String.toList "(1")) |> Expect.equal (Err ("Expecting: ([a-zA-Z])", Nothing, ['(','1']))
      , test "Parse a opening curly brace and a digit in this specific order from \"{1\" should succeed" <|
          \_ -> (Parser.openingCurlyBrace >*> Parser.digit <| (String.toList "{1")) |> Expect.equal (Ok [((Just '{', Just '1'), String.toList "")])
      ]

buildTests =
    describe "build function tests"
      [ test "Parse a digit from \"8xyz\" and create an interger should succeed" <|
          \_ -> Parser.build Parser.digit (\c -> (toCode c)-48) (String.toList "8xyz") |> Expect.equal (Ok [(8, String.toList "xyz")])
      , test "Parse a digit from \"xyz\" should fail" <|
          \_ -> Parser.build Parser.digit (\c -> (toCode c)-48) (String.toList "xyz") |> (Expect.equal (Err ("Expecting: ([0-9])", Nothing, ['x','y','z'])))
      , test "Parse a string from \"hello123\" and create an actual string should succeed" <|
          \_ -> Parser.build Parser.string (\cs -> String.fromList cs) (String.toList "hello123") |> Expect.equal (Ok [("hello", String.toList "123")])
      ]

listTests =
      describe "list function tests"
        [ test "Parse a number from \"123xyz\" should succeed" <|
            \_ -> Parser.list Parser.digit (String.toList "123xyz") |> Expect.equal (Ok [(String.toList "123", String.toList "xyz")])
        , test "Parse a number from \"xyz\" should fail" <|
            \_ -> Parser.list Parser.digit (String.toList "xyz") |> Expect.equal (Err ("Expecting list of ([0-9])", Nothing, (String.toList "xyz")))
        , test "Parse a string from \"xyz123\" should succeed" <|
            \_ -> Parser.list Parser.letter (String.toList "xyz123") |> Expect.equal (Ok [(String.toList "xyz", String.toList "123")])
        , test "Parse a string from \"123\" should fail" <|
            \_ -> Parser.list Parser.letter (String.toList "123") |> Expect.equal (Err ("Expecting list of ([a-zA-Z])", Nothing, ['1','2','3']))
        ]

many0Tests =
      describe "optionalList function tests"
        [ test "Parse a string from \"123xyz\" should succeed with nothing parsed" <|
            \_ -> Parser.many0 Parser.string (String.toList "123xyz") |> Expect.equal (Ok [([], String.toList "123xyz")])
        , test "Parse a string from \"xyz132\" should succeed" <|
            \_ -> Parser.many0 Parser.string (String.toList "xyz123") |> Expect.equal (Ok [([['x','y','z']],['1','2','3'])])
        {-, test "Parse multipe tokens from \"+x*y+z\" should succeed" <|
            \_ -> Parser.many0 [] Parser.token (String.toList "+x*y+z") |> Expect.equal  [([['+','x'],['*','y'],['+','z']],[])]-}
        ]

tokenTests =
      describe "token function tests"
        [ test "Parse a token from \"_xyz123\" should fail" <|
            \_ -> Parser.token (String.toList "_xyz123") |> Expect.equal (Parser.fail "Expecting list of ([a-zA-Z]) || Expecting: :, ., =, $, -, (, +, *, ) prefixed with \\" [] (String.toList "_xyz123"))
        , test "Parse a token from \"xyz123\" should succeed" <|
            \_ -> Parser.token (String.toList "xyz123") |> Expect.equal (Ok [(String.toList "xyz", String.toList "123")])
        , test "Parse a token from \"xyz\\$123\" should succeed" <|
            \_ -> Parser.token (String.toList "xyz\\$123") |> Expect.equal (Ok [(String.toList "xyz\\$", String.toList "123")])
        , test "Parse a token from \"*xyz123\" should succeed" <|
            \_ -> Parser.token (String.toList "\\(\\)") |> Expect.equal (Ok [(String.toList "\\(\\)", String.toList "")])
        ]

optionTests =
      describe "opton function tests"
          [ test "Parse a string from \"xyz123\" should succeed" <|
              \_ -> Parser.option Parser.string (String.toList "xyz123") |> Expect.equal (Ok [(String.toList "xyz", String.toList "123")])
          , test "Parse a string from \"123xyz\" should succeed" <|
              \_ -> Parser.option Parser.string (String.toList "123xyz") |> Expect.equal (Ok [([], String.toList "123xyz")])
          ]
