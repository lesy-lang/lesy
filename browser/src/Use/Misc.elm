module Use.Misc exposing (..)

import Html
import Html.Attributes as HtmlAttr
import Element as Ui
import Dict exposing (Dict)
import Json.Decode exposing (index)
import Set
import Maybe exposing (withDefault)
import Array
import Json.Encode
import Use.Json exposing (encodeDict)

{-|convert 0..1 with 1 being 100% to percent string

    oneAs100Percent 1.5 --"150%"
-}
oneAs100Percent: Float ->String
oneAs100Percent f1=
  (String.fromFloat ((*) 100 f1))
  ++"%"

noTextSelect: Html.Attribute msg
noTextSelect=
  HtmlAttr.style
    "-webkit-user-select" "none"


smallestFreeIndex: List Int ->Int
smallestFreeIndex list=
  list
    |>Set.fromList
    |>Set.diff
        (List.range 0 ((-) (List.length list) 1)
        |>Set.fromList
        )
    |>Set.toList
  |>List.head
  |>Maybe.withDefault (List.length list)
