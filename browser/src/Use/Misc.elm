module Use.Misc exposing (..)

import Html
import Html.Attributes as HtmlAttr
import Element as Ui
import Dict exposing (Dict)
import Json.Decode exposing (index)
import Set

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
  let potentialClashing=
        list
        |>List.filter
            (\index-> index < (List.length list))
        |>Set.fromList
  in
  List.range 0 (List.length list)
  |>Set.fromList
  |>Set.diff potentialClashing
  |>Set.foldr min (List.length list)
