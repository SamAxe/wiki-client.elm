module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, spaces)
import Html exposing (text)

import WikiHelper
import Wiki

type alias Point =
  { x : Float
  , y : Float
  }

point : Parser Point
point =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"



suite : Test
suite =
  test "A first test" <|
    \_ -> Expect.equal ( Ok { x = 3, y = 14 }) ( Parser.run point "(3,14)")

hello_words : Test
hello_words =
  test "Parse some plain words" <|
    \_ -> Expect.equal ( Ok ("hello world") ) ( Parser.run WikiHelper.plain_words "hello world")


hello_words1 : Test
hello_words1 =
  test "Parse some link words" <|
    \_ -> Expect.equal (Ok ( WikiHelper.Link ("hello world") ) ) ( Parser.run WikiHelper.internal_link_or_word "[[hello world]]")

hello_words2 : Test
hello_words2 =
  test "Parse some link words 2" <|
    \_ -> Expect.equal (Ok ( WikiHelper.PlainText ("[hello world") ) ) ( Parser.run WikiHelper.internal_link_or_word "[hello world")

hello_words3 : Test
hello_words3 =
  test "Parse some link words 3" <|
    \_ -> Expect.equal (Ok ( [ WikiHelper.Link ("hello world"), WikiHelper.PlainText " ", WikiHelper.Link ("hello world") ] ) ) ( Parser.run WikiHelper.internal_link_or_words "[[hello world]] [[hello world]]")



slug_test : Test
slug_test =
  test "test some slugs" <|
    \_ -> Expect.equal ( Wiki.Slug "welcome-visitors") ( Wiki.asSlug "Welcome Visitors" )

