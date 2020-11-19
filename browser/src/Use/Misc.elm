module Use.Misc exposing (..)

import Html
import Html.Attributes as HtmlAttr
import Element as Ui

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
