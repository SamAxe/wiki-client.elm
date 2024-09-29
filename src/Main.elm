module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode

import Browser.Navigation as Nav
import Url
import Url.Parser exposing ( (</>) )
-- import Url.Route

import WikiHelper
import Wiki


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
  { key : Nav.Key
  , url : Url.Url
  , story : Story
  }


type alias Paragraph =
  { type_ : String
  , id    : String
  , text  : Maybe String
  }

type alias Story =
  { title  : String
  , story  : List Paragraph
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
--  ( Model key url { title = "blank", story = [] }, Cmd.none )
  ( Model key url { title = "blank", story = [] }, getRandomStory (Wiki.asSlug "Welcome Visitors"))


--  (Loading, getRandomStory)




-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotStory (Result Http.Error Story)


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
  let x = Url.Parser.parse routeParser url in
      Debug.log (fred x )
      x

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , getRandomStory ( (Maybe.withDefault (Wiki.asSlug "Welcome Visitors") (doit url)) )
      )

    GotStory result ->
      case result of
        Ok story ->
          ( { model | story = story }, Cmd.none)

        Err _ -> ( model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


-- view : Model -> Browser.Document Msg
-- view model =
--   { title = "URL Interceptor"
--   , body =
--       [ text "The current URL is: "
--       , b [] [ text (Url.toString model.url) ]
--       , ul []
--           [ viewLink "/home"
--           , viewLink "/profile"
--           , viewLink "/reviews/the-century-of-the-self"
--           , viewLink "/reviews/public-opinion"
--           , viewLink "/reviews/shah-of-shahs"
--           ]
--       ]
--   }
-- 
-- 
-- viewLink : String -> Html msg
-- viewLink path =
--   li [] [ a [ href path ] [ text path ] ]
-- 


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body = [ div []
    [ text "The current URL is: "
    , b [] [ text (Url.toString model.url) ]
    , h2 [] [ text "Random Story" ]
    , viewStory model
    ]
    ]
  }

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
      div []
        [ h1 [] [ text model.story.title ]
        , div [] (List.map viewParagraph model.story.story)
--        , p [ style "text-align" "right" ]
--            [ text "— "
--            , cite [] [ text story.source ]
--            , text (" by " ++ story.author ++ " (" ++ String.fromInt story.year ++ ")")
--            ]
        ]



-- HTTP


getRandomStory : Wiki.Slug -> Cmd Msg
getRandomStory (Wiki.Slug slugname) =
  Debug.log slugname
  Http.get
    --{ url = "https://elm-lang.org/api/random-quotes"
    { url = "http://fed.wiki/" ++ slugname ++ ".json"
    , expect = Http.expectJson GotStory storyDecoder
    }

paragraphDecoder : Json.Decode.Decoder Paragraph
paragraphDecoder =
  Json.Decode.map3 Paragraph
    (Json.Decode.field "type" Json.Decode.string)
    (Json.Decode.field "id" Json.Decode.string )
    (Json.Decode.maybe (Json.Decode.field "text" Json.Decode.string ))

storyDecoder : Json.Decode.Decoder Story
storyDecoder =
  Json.Decode.map2 Story
    (Json.Decode.field "title" Json.Decode.string)
    (Json.Decode.field "story" (Json.Decode.list paragraphDecoder))
--    (Json.Decode.field "source" Json.Decode.string)
--    (Json.Decode.field "author" Json.Decode.string)
--    (Json.Decode.field "year" int)
