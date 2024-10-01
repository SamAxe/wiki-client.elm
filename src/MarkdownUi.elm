module MarkdownUi exposing (view)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html exposing (Attribute, Html)
import Html.Attributes
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


buildToc : List Block -> TableOfContents
buildToc blocks =
    let
        headings =
            gatherHeadings blocks
    in
    headings
        |> List.map Tuple.second
        |> List.map
            (\styledList ->
                { anchorId = styledList |> inlinesToId
                , name = styledToString styledList
                , level = 1
                }
            )


styledToString : List Inline -> String
styledToString inlines =
    --List.map .string list
    --|> String.join "-"
    -- TODO do I need to hyphenate?
    inlines
        |> Block.extractInlineText


inlinesToId : List Inline -> String
inlinesToId list =
    list
        |> Block.extractInlineText
        |> Debug.log "extracted"
        |> String.split " "
        |> Debug.log "split"
        |> String.join "-"
        |> Debug.log "joined"


gatherHeadings : List Block -> List ( Block.HeadingLevel, List Inline )
gatherHeadings blocks =
    List.filterMap
        (\block ->
            case block of
                Block.Heading level content ->
                    Just ( level, content )

                _ ->
                    Nothing
        )
        blocks


type alias TableOfContents =
    List { anchorId : String, name : String, level : Int }


view : String -> Result String (List (Element msg))
view markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render renderer)


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = \value -> Element.paragraph [] [ Element.text value ]
    , strong = \content -> Element.paragraph [ Font.bold ] content
    , emphasis = \content -> Element.paragraph [ Font.italic ] content
    , strikethrough = \content -> Element.paragraph [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink []
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
                        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.paragraph
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.paddingXY 10 0 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.paragraph []
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Font.bold
            , Element.width Element.fill
            , Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , html = Markdown.Html.oneOf []
    }


alternateTableRowBackground =
    Element.rgb255 245 247 249


tableBorder =
    [ Element.Border.color (Element.rgb255 223 226 229)
    , Element.Border.width 1
    , Element.Border.solid
    , Element.paddingXY 6 13
    , Element.height Element.fill
    ]


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


heading : { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Block.H1 ->
                    (36//2)

                Block.H2 ->
                    (24//2)

                _ ->
                    (20//2)
            )
        , Font.bold
--        , Font.family [ Font.typeface "Montserrat" ]
        , Element.Region.heading (Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.paragraph
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        [ Element.text details.body ]
