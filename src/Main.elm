module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element.Background as Background
import Element.Border as Border
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- import Html exposing (..)
import Http
import Json.Decode
import MarkdownUi
import Parser exposing (Parser, (|.), (|=) )
import Url
import Url.Parser exposing ( (</>) )
-- import Url.Route
import Wiki
import Element.Input as Input
import List exposing (tail)
import Internal.Model exposing (Description(..))

-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key       : Nav.Key
  , url       : Url.Url
  , stories   : List Story
  , ptext     : Maybe String
  , pselected : Maybe ParaId
  }


type alias Paragraph =
  { type_ : String
  , id    : Maybe ParaId
  , text  : Maybe String
  }

type alias Story =
  { title  : String
  , story  : List Paragraph
  }

type ParaId = ParaId String

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
--  ( Model key url { title = "blank", story = [] }, Cmd.none )
  ( Model key url [ ] Nothing Nothing, getRandomStory (Wiki.asSlug "Welcome Visitors"))


--  (Loading, getRandomStory)




-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotStory (Result Http.Error Story)
  | TextChanged String
  | SelectParagraph (Maybe ParaId)
  | SaveEditItem
  | CancelEditItem


-- type Route =
  -- RouteSlug String


--routeParser : Url.Parser.Parser (Route -> a) a
routeParser : Url.Parser.Parser (Wiki.Slug -> a) a
routeParser =
  -- Url.Parser.map Wiki.Slug   (Url.Parser.s "src" </> Url.Parser.string)
  Url.Parser.map Wiki.Slug   (Url.Parser.string)

fred : Maybe Wiki.Slug -> String
fred slug =
  case slug of
      Just (Wiki.Slug sname) -> "Just " ++ sname
      Nothing -> "nothing"

doit : Url.Url -> Maybe Wiki.Slug
doit url =
  Url.Parser.parse routeParser url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          Debug.log ("Internal: " ++ Url.toString url)
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          Debug.log ("external " ++ href)
          ( model, Nav.load href )

    UrlChanged url ->
      Debug.log ("Changed: " ++ Url.toString url)
      ( { model | url = url }
      , getRandomStory ( (Maybe.withDefault (Wiki.asSlug "Welcome Visitors") (doit url)) )
      )

    GotStory result ->
      case result of
        Ok story ->
          ( { model | stories = (story :: model.stories) }, Cmd.none)

        Err _ -> ( model, Cmd.none)

    TextChanged pt ->
        ( { model | ptext = Just pt}, Cmd.none)

    SelectParagraph para_id ->
      ( { model | pselected = para_id, ptext = getSelection para_id model.stories}, Cmd.none)

    SaveEditItem -> 
      ( { model | stories = updateSelection model.pselected model.ptext model.stories
        , ptext = Nothing
        , pselected = Nothing
        }
      , Cmd.none )

    CancelEditItem -> 
      ( { model 
        | ptext = Nothing
        , pselected = Nothing
        }
      , Cmd.none )



getSelection : Maybe ParaId -> List Story -> Maybe String
getSelection paraId stories =
  case stories of
    [] -> Nothing
    hd::tail ->
      case getParagraphSelection paraId hd.story of
          Nothing -> getSelection paraId tail
          Just p  -> Just p

getParagraphSelection : Maybe ParaId -> List Paragraph -> Maybe String
getParagraphSelection paraId paragraphs =
  case paragraphs of
    [] -> Nothing
    hd::tail ->
      if hd.id == paraId
      then hd.text
      else getParagraphSelection paraId tail


updateSelection : Maybe ParaId -> Maybe String -> List Story -> List Story
updateSelection paraId new_text stories =
  List.map (updateStory paraId new_text) stories

updateStory : Maybe ParaId -> Maybe String -> Story -> Story
updateStory paraId new_text story =
  { story | story = List.map (updateParagraph paraId new_text) story.story }


updateParagraph : Maybe ParaId -> Maybe String -> Paragraph -> Paragraph
updateParagraph paraId new_text paragraph =
  if paraId == paragraph.id
  then { paragraph | text = new_text }
  else paragraph


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
      [ layout [ width (px (350*4+30*3)), height fill ] <|
          column [ width fill, scrollbarX]
              [ header
              , (viewStories model)
              , footer
              ]
      ]

    }

color : { blue : Color, darkCharcoal : Color, lightBlue : Color, lightGrey : Color, white : Color }
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }

logo : Element msg
logo =
    el
        [ width <| px 80
        , height <| px 40
        , Border.width 2
        , Border.rounded 6
        , Border.color color.blue
        ]
        none


header : Element msg
header =
    row [ width fill, padding 20, spacing 20 ]
        [ logo
        , el [ alignRight ] <| text "Services"
        , el [ alignRight ] <| text "About"
        , el [ alignRight ] <| text "Contact"
        ]


footer : Element msg
footer =
    row
        [ width fill
        , padding 10
        , Background.color color.lightBlue
        , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , Border.color color.blue
        ]
        [ logo
        , column [ alignRight, spacing 10 ]
            [ el [ alignRight ] <| text "Services"
            , el [ alignRight ] <| text "About"
            , el [ alignRight ] <| text "Contact"
            ]
        ]



paragraph_text : Paragraph -> String
paragraph_text paragraph =
  case (paragraph.text) of
    Just t -> t
    Nothing -> "<no text>"

editItem : Maybe String -> Element Msg
editItem paragraph =
 Element.column [width fill, Background.color color.lightGrey, Border.rounded 6]
   [ row [width fill]
      [ Input.button [Element.padding 10,Font.bold, Font.underline, Font.color (rgb255 0x72 0x9F 0xCF) ] {onPress = Just SaveEditItem, label = Element.text "Save"}
      , Input.button [Element.padding 10,Font.bold, Font.underline, Font.color (rgb255 0x72 0x9F 0xCF) ] {onPress = Just CancelEditItem, label = Element.text "Cancel"}
      ]
   , Input.multiline [ Border.rounded 4, width fill, Background.color color.lightGrey, Border.width 2 ] 
      { onChange = TextChanged 
      , text = Maybe.withDefault "" paragraph
      , placeholder = Nothing 
      , label = Input.labelHidden "default is fill?"
      , spellcheck = True }
  ]

editParagraph : Maybe String -> Element Msg
editParagraph paragraph =
  editItem paragraph

viewParagraph1 : Paragraph -> Element Msg
viewParagraph1 paragraph =
  case paragraph.type_ of
    "paragraph" -> paragraph |> resolveLinks
    "markdown"  -> 
        (case MarkdownUi.view (paragraph |> paragraph_text |> resolveLinks_markdown ) of
            Ok rendered ->
                Element.column
                    [ Element.spacing 30
                    , Element.padding 10
                    ]
                    rendered

            Err errors ->
                Element.text errors
        )



    _ -> paragraph |> resolveLinks


viewParagraph : Model -> Paragraph -> Element Msg
viewParagraph model paragraph =
  if paragraph.id == model.pselected
  then editParagraph model.ptext
  else viewParagraph1 paragraph

viewStoryButtonHeader =
  row [width fill]
    [ Input.button [Element.padding 10,Font.bold, Font.underline, Font.color color.lightGrey ] {onPress = Nothing, label = Element.text "Close"}
    ]

viewStory : Model -> Story -> Element Msg
viewStory model story =
  let 
    borderedColumn =
      column
        [ width (px 350)
        , height (px 500)
        , scrollbarY
        , padding 5
        , spacing 5
        , Font.size 16
        , Border.width 2
        , Border.rounded 6
        , scrollbarY
        , Border.color color.blue
        ]
  in

        borderedColumn 
        ( viewStoryButtonHeader
        :: (Element.el [ centerX, padding 30, Font.bold, Font.size 24 ] (Element.text story.title))
        :: (List.map (viewParagraph model) story.story))


viewStories : Model -> Element Msg
viewStories model =
      Element.row [ spacing 10, padding 20 ]
        (List.map (viewStory model) (List.reverse model.stories))

-- HTTP


getRandomStory : Wiki.Slug -> Cmd Msg
getRandomStory (Wiki.Slug slugname) =
  Debug.log ( "getRandomStory: " ++ slugname)
  Http.get
    --{ url = "https://elm-lang.org/api/random-quotes"
    -- { url = "http://fed.wiki/" ++ slugname ++ ".json"
    { url = "http://localhost:3000/" ++ slugname ++ ".json"
    , expect = Http.expectJson GotStory storyDecoder
    }


paragraphDecoder : Json.Decode.Decoder Paragraph
paragraphDecoder =
  Json.Decode.map3 Paragraph
    (Json.Decode.field "type" Json.Decode.string)
    (Json.Decode.maybe (Json.Decode.field "id" (Json.Decode.map ParaId Json.Decode.string)))
    (Json.Decode.maybe (Json.Decode.field "text" Json.Decode.string ))

storyDecoder : Json.Decode.Decoder Story
storyDecoder =
  Json.Decode.map2 Story
    (Json.Decode.field "title" Json.Decode.string)
    (Json.Decode.field "story" (Json.Decode.list paragraphDecoder))
--    (Json.Decode.field "source" Json.Decode.string)
--    (Json.Decode.field "author" Json.Decode.string)
--    (Json.Decode.field "year" int)


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


textChanged : String -> Msg
textChanged str =
  TextChanged str

para_to_html : Para -> Element Msg
para_to_html p =
  case p of
    Link link_string -> 
      let slug = Wiki.asSlug link_string in
--        Html.a [ Html.Attributes.href (fw_url slug) ] [ Html.text link_string ]
        Element.link [ Font.bold, Font.underline, Font.color (rgb255 0x72 0x9F 0xCF) ] { url = (fw_url slug), label = Element.text link_string }

    PlainText text_string -> Element.text text_string
-- { onChange : String -> msg, text : String, placeholder : Maybe (Input.Placeholder msg), label : Input.Label msg } -> Element msg
--    PlainText text_string -> Input.text [ Border.rounded 4 ] { onChange = TextChanged , text = text_string , placeholder = Nothing , label = Input.labelAbove [] <| text "default is fill?" }

para_to_markdown : Para -> String
para_to_markdown p =
  case p of
    Link link_string -> 
      let slug = Wiki.asSlug link_string in
        "[" ++ link_string ++ "](" ++ (fw_url slug) ++ ")"

    PlainText text_string -> text_string


text_to_html : Maybe ParaId -> String -> Element Msg
text_to_html para_id para_text =
  case Parser.run internal_link_or_words para_text of
    Ok para_ast -> Element.paragraph [padding 10, Events.onDoubleClick (SelectParagraph para_id) ] (List.map para_to_html para_ast)
    Err _ -> Element.text "parse error: "


text_to_markdown : String -> String
text_to_markdown para_text =
  case Parser.run internal_link_or_words para_text of
    Ok para_ast -> (List.map para_to_markdown para_ast) |> String.join " "
    Err _ -> "parse error: "


resolveLinks : Paragraph -> Element Msg
resolveLinks wiki_paragraph =
    text_to_html wiki_paragraph.id (Maybe.withDefault "" wiki_paragraph.text)

resolveLinks_markdown : String -> String
resolveLinks_markdown wiki_text =
    wiki_text
    |> text_to_markdown

