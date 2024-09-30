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

link_words : Test
link_words =
  describe "Test internal linking words with [[ ... ]] notations"
  [ test "Parse some plain words" <|
      \_ -> Expect.equal ( Ok ("hello world") ) ( Parser.run WikiHelper.plain_words "hello world")
  , test "Parse some link words" <|
    \_ -> Expect.equal (Ok ( WikiHelper.Link ("hello world") ) ) ( Parser.run WikiHelper.internal_link_or_word "[[hello world]]")
  , test "Parse some link words 2" <|
    \_ -> Expect.equal (Ok ( WikiHelper.PlainText ("[hello world") ) ) ( Parser.run WikiHelper.internal_link_or_word "[hello world")
  , test "Parse some link words 3" <|
    \_ -> Expect.equal (Ok ( [ WikiHelper.Link ("hello world"), WikiHelper.PlainText " ", WikiHelper.Link ("hello world") ] ) ) ( Parser.run WikiHelper.internal_link_or_words "[[hello world]] [[hello world]]")
  , test "Parse some link words 4" <|
    \_ -> Expect.equal (Ok ( [ WikiHelper.PlainText ("add more writing spaces. Read "), WikiHelper.Link "How to Wiki", WikiHelper.PlainText (" for more ideas. Follow "), WikiHelper.Link "Recent Changes", WikiHelper.PlainText " here and nearby." ] ) ) ( Parser.run WikiHelper.internal_link_or_words "add more writing spaces. Read [[How to Wiki]] for more ideas. Follow [[Recent Changes]] here and nearby.")
  , test "Parse some link words 5" <|
    \_ -> Expect.equal (Ok ( [ WikiHelper.PlainText "You can edit your copy of these pages.  Press ", WikiHelper.PlainText "[+] to add more writing spaces. Read ", WikiHelper.Link "How to Wiki", WikiHelper.PlainText (" for more ideas. Follow "), WikiHelper.Link "Recent Changes", WikiHelper.PlainText " here and nearby." ] ) ) ( Parser.run WikiHelper.internal_link_or_words "You can edit your copy of these pages. Press [+] to add more writing spaces. Read [[How to Wiki]] for more ideas. Follow [[Recent Changes]] here and nearby.")
  ]




slug_test : Test
slug_test =
  test "test some slugs" <|
    \_ -> Expect.equal ( Wiki.Slug "welcome-visitors") ( Wiki.asSlug "Welcome Visitors" )


