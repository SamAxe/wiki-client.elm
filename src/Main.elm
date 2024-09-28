module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map, field, int, string)

import WikiHelper


-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL


type Model
  = Failure
  | Loading
  | Success Story

type alias Paragraph =
  { type_ : String
  , id    : String
  , text  : Maybe String
  }

type alias Story =
  { title  : String
  , story  : List Paragraph
  }


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getRandomStory)



-- UPDATE


type Msg
  = MorePlease
  | GotStory (Result Http.Error Story)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomStory)

    GotStory result ->
      case result of
        Ok story ->
          (Success story, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Story" ]
    , viewStory model
    ]

viewParagraph : Paragraph -> Html Msg
viewParagraph paragraph =
  let
    para_text =
      case (paragraph.text) of
        Just t ->
          WikiHelper.resolveLinks t

        Nothing ->
          text "<no text>"
  in

  div [] [ h3 [] [ 
           span [] [text paragraph.type_]
         , span [style "color" "red"] [ text "id:"]
         , span [] [ text paragraph.id ]
         ]
         , para_text
         ]


viewStory : Model -> Html Msg
viewStory model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random story for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success story ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , h1 [] [ text story.title ]
        , div [] (List.map viewParagraph story.story)
--        , p [ style "text-align" "right" ]
--            [ text "— "
--            , cite [] [ text story.source ]
--            , text (" by " ++ story.author ++ " (" ++ String.fromInt story.year ++ ")")
--            ]
        ]



-- HTTP


getRandomStory : Cmd Msg
getRandomStory =
  Http.get
    --{ url = "https://elm-lang.org/api/random-quotes"
    { url = "http://fed.wiki/welcome-visitors.json"
    , expect = Http.expectJson GotStory storyDecoder
    }

paragraphDecoder : Decoder Paragraph
paragraphDecoder =
  Json.Decode.map3 Paragraph
    (field "type" string)
    (field "id" string )
    (Json.Decode.maybe (field "text" string ))

storyDecoder : Decoder Story
storyDecoder =
  Json.Decode.map2 Story
    (field "title" string)
    (field "story" (Json.Decode.list paragraphDecoder))
--    (field "source" string)
--    (field "author" string)
--    (field "year" int)
