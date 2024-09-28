module Wiki exposing (..)

import Regex
import Html exposing (text)

type Slug = Slug String

asSlug : String -> Slug
asSlug name =

  let
    replaceWhitespaceRegEx =
      Maybe.withDefault Regex.never <| Regex.fromString "[\n\r\t ]"
    replaceLettersAndDigitsEx =
      Maybe.withDefault Regex.never <| Regex.fromString "[^A-Za-z0-9-]"


    replaceWhitespaceReg str =
      Regex.replace replaceWhitespaceRegEx ( \_ -> "-") str

    replaceLettersAndDigits str =
      Regex.replace replaceLettersAndDigitsEx ( \_ -> "") str
  in

    name
    |> replaceWhitespaceReg
    |> replaceLettersAndDigits
    |> String.toLower
    |> Slug

asTitle : Slug -> String
asTitle (Slug slug_name) =
  let
    replaceDashesEx =
      Maybe.withDefault Regex.never <| Regex.fromString "-"
    replaceDashes str =
      Regex.replace replaceDashesEx ( \_ -> " ") str
  in
    slug_name
    |> replaceDashes


