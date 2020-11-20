module Use.Html exposing (..)

import Html
import Html.Attributes as HtmlAttr


noTextSelect: Html.Attribute msg
noTextSelect=
  HtmlAttr.style
    "-webkit-user-select" "none"

