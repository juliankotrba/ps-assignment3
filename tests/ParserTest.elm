module ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser exposing ((>*>))
import Char exposing (toCode)

suceedTests =
    describe "succeed function tests"
      [ test "suceed with value 'x' and input [] should succeed" <|
          \_ -> Parser.succeed 'x' [] |> Expect.equal [('x',[])]
      , test "suceed with value 'x' and input [y','z'] should succeed" <|
          \_ -> Parser.succeed 'x' ['y','z'] |> Expect.equal [('x',['y','z'])]
      , test "suceed with value 'x' and input ['x','y','z'] should succeed" <|
          \_ -> Parser.succeed 'x' ['x','y','z'] |> Expect.equal [('x',['x','y','z'])]
      , test "suceed with value 1 and input [1,2,3] should succeed" <|
          \_ -> Parser.succeed 'x' ['x','y','z'] |> Expect.equal [('x',['x','y','z'])]
      , test "suceed with value 1 and input [2,3] should succeed" <|
          \_ -> Parser.succeed 'x' ['x','y','z'] |> Expect.equal [('x',['x','y','z'])]
      ]

failTests =
    describe "fail function tests"
      [ test "fail with input [] should fail" <|
          \_ -> Parser.fail [] |> Expect.equal []
      , test "fail with input ['a', 'b', c''] should fail" <|
          \_ -> Parser.fail [] |> Expect.equal []
      , test "fail with input [1,2,3] should fail" <|
          \_ -> Parser.fail [] |> Expect.equal []
      ]

tokenTests =
    describe "token function tests"
        [ test "Parse token 'x' from [] should fail" <|
            \_ -> Parser.token 'x' []  |> Expect.equal []
        , test "Parse token 'x' from ['a','b','c'] should fail" <|
            \_ -> Parser.token 'x' ['a','b','c'] |> Expect.equal []
        , test "Parse token 'x' from ['x','b','c'] should succeed" <|
            \_ -> Parser.token 'x' ['x','b','c'] |> Expect.equal [('x', ['b', 'c'])]
        , test "Parse token 1 from [] should fail" <|
            \_ -> Parser.token 1 []  |> Expect.equal []
        , test "Parse token from [2, 3] should fail" <|
            \_ -> Parser.token 1 [2, 3]  |> Expect.equal []
        , test "Parse token 1 from [1, 2, 3] should succeed" <|
            \_ -> Parser.token 1 [1, 2]  |> Expect.equal [(1, [2])]
        ]

spotTests =
    describe "spot function tests"
        [ test "The token function is now just calling the spot function with an equals check lambda function as an argument. So the tokenTests are already testing the spot implementation." <|
            \_ -> "xyz"  |> Expect.equal "xyz"
        ]

altTests =
    describe "alt function tests"
      [ test "Parse a digit or a letter from \"(xyz\" should fail" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "(xyz") |> Expect.equal []
      , test "Parse a digit or a letter from \"xyz\" should succeed" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "xyz") |> Expect.equal [('x',String.toList "yz")]
      , test "Parse a digit or a letter from \"1yz\" should succeed" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "1yz") |> Expect.equal [('1',String.toList "yz")]
      , test "Parse a digit or a letter from an empty string should fail" <|
          \_ -> Parser.alt Parser.digit Parser.letter (String.toList "") |> Expect.equal []
      , test "Parse a number or a string from \"xyz123\" should succeed" <|
          \_ -> Parser.alt Parser.number Parser.string (String.toList "xyz123") |> Expect.equal [(String.toList "xyz", String.toList "123")]    
      ]

seqTests =
    describe ">*> function tests"
      [ test "Parse a digit and a letter in this specific order from \"1(\" should fail" <|
          \_ -> ((Parser.digit >*> Parser.letter) <| (String.toList "1(")) |> Expect.equal []
      , test "Parse a letter and a digit in this specific order from \"(1\" should fail" <|
          \_ ->  (Parser.letter >*> Parser.digit <| (String.toList "(1")) |> Expect.equal []
      , test "Parse a opening curly brace and a digit in this specific order from \"{1\" should succeed" <|
          \_ ->  (Parser.openingCurlyBrace >*> Parser.digit <| (String.toList "{1")) |> Expect.equal [(('{','1'), String.toList "")]
      ]

buildTests =
    describe "build function tests"
      [ test "Parse a digit from \"8xyz\" and create an interger should succeed" <|
          \_ ->  Parser.build Parser.digit (\c -> (toCode c)-48) (String.toList "8xyz") |> Expect.equal [(8, String.toList "xyz")]
      , test "Parse a digit from \"xyz\" should fail" <|
          \_ ->  Parser.build Parser.digit (\c -> (toCode c)-48) (String.toList "xyz") |> Expect.equal []
      , test "Parse a string from \"hello123\" and create an actual string should succeed" <|
          \_ ->  Parser.build Parser.string (\cs -> String.fromList cs) (String.toList "hello123") |> Expect.equal [("hello", String.toList "123")]
      ]

listTests =
      describe "list function tests"
        [ test "Parse a number from \"123xyz\" should succeed" <|
            \_ -> Parser.list [] Parser.digit (String.toList "123xyz") |> Expect.equal [(String.toList "123", String.toList "xyz")]
        , test "Parse a number from \"xyz\" should fail" <|
            \_ -> Parser.list [] Parser.digit (String.toList "xyz") |> Expect.equal []
        , test "Parse a string from \"xyz123\" should succeed" <|
            \_ -> Parser.list [] Parser.letter (String.toList "xyz123") |> Expect.equal [(String.toList "xyz", String.toList "123")]
        , test "Parse a string from \"123\" should fail" <|
            \_ -> Parser.list [] Parser.letter (String.toList "123") |> Expect.equal []
        ]

optionalListTests =
      describe "optionalList function tests"
        [ test "Parse a string from \"123xyz\" should succeed with nothing parsed" <|
            \_ -> Parser.optionalList Parser.string (String.toList "123xyz") |> Expect.equal [([], String.toList "123xyz")]
        , test "Parse a string from \"xyz132\" should succeed" <|
            \_ -> Parser.optionalList Parser.string (String.toList "xyz123") |> Expect.equal [(String.toList "xyz", String.toList "123")]

        ]
