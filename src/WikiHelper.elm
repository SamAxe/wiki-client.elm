module WikiHelper exposing (..)

import Parser exposing (Parser, (|.), (|=) )
import Html exposing (..)

import Browser
import Browser.Navigation as Nav
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
import Url
import Element exposing (..)


import Wiki
import Element.Font exposing (underline)

type Para =
    PlainText String
  | Link      String


plain_words_helper : String -> Parser String
plain_words_helper link_text =
  if String.isEmpty link_text
  then
    Parser.problem "plain words cannot be 0 length"
  else
    Parser.succeed link_text

plain_parse_to_next_link : Parser String
plain_parse_to_next_link =
  Parser.getChompedString (
    Parser.chompWhile (\c -> c /= '[' && c /= '\n' && c /= '\r') )

start_plain_parse : Parser ()
start_plain_parse =
  Parser.succeed ()
  |. Parser.chompIf (\c -> c == '[' )

plain_words : Parser String
plain_words =
  plain_parse_to_next_link
  |> Parser.andThen plain_words_helper

plain_words2 : Parser String
plain_words2 =
  Parser.getChompedString (
    Parser.chompWhile (\c -> c /= '\n' && c /= '\r') )
    |> Parser.andThen plain_words_helper

link_words : Parser String
link_words =
  Parser.getChompedString <|
    Parser.succeed ()
      |. Parser.chompWhile (\c -> c /= ']' )

internal_words : Parser Para
internal_words =
  Parser.succeed PlainText
    |= plain_words

internal_words2 : Parser Para
internal_words2 =
  Parser.succeed PlainText
    |= plain_words2


internal_link : Parser Para
internal_link =
  Parser.succeed Link
    |. Parser.symbol "[["
    |= link_words
    |. Parser.symbol "]]"

internal_link_or_word : Parser Para
internal_link_or_word =
  Parser.oneOf 
    [ internal_link
    , internal_words
    , internal_words2
    ]

internal_link_or_words : Parser (List Para)
internal_link_or_words =
  Parser.loop [] internal_link_or_words_helper

internal_link_or_words_helper : List Para -> Parser (Parser.Step (List Para) (List Para))
internal_link_or_words_helper revParas =
  Parser.oneOf
    [ Parser.succeed (\para -> Parser.Loop (para :: revParas))
        |= internal_link_or_word
    , Parser.succeed ()
        |> Parser.map (\_ -> Parser.Done (List.reverse revParas))
    ]


-- "http://fed.wiki/welcome-visitors.json

fw_url : Wiki.Slug -> String
fw_url (Wiki.Slug slugname) =
--  "http://fed.wiki/" ++ slugname ++ ".json"
  "/" ++ slugname


para_to_html : Para -> Element msg
para_to_html p =
  case p of
    Link link_string -> 
      let slug = Wiki.asSlug link_string in
--        Html.a [ Html.Attributes.href (fw_url slug) ] [ Html.text link_string ]
        Element.link [ Element.Font.bold, underline] { url = (fw_url slug), label = Element.text link_string }

    PlainText text_string -> Element.text text_string


text_to_html : String -> Element msg
text_to_html paragraph_text =
  case Parser.run internal_link_or_words paragraph_text of
    Ok para_ast -> Element.paragraph [padding 10] (List.map para_to_html para_ast)
    Err _ -> Element.text "parse error: "

resolveLinks : String -> Element msg
resolveLinks wiki_text =
    wiki_text
    |> text_to_html

