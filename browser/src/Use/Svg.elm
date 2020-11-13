module Use.Svg exposing
  ( Size, relative, absolute
  , width, height
  , translate

  , Color, transparent, rgb, rgba
  , fillColor, lineColor

  , group

  , circle, polygon
  , line
  , lineWidth
  , Linecap, roundCap, linecap
  , Linejoin, roundJoin, linejoin
  
  , text, fontFamily
  , fontSize
  , aproximateMonospacedWidth, computeHeight

  , Cursor, finger, hand, arrow, cursor

  , mouseMoves
  )

import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Use.Translate exposing (Translate, xy)

import Json.Decode


text: String ->List (Svg.Attribute msg) ->Svg msg
text content attrs=
  Svg.text_ attrs [ Svg.text content ]


type Size=
  Size String

{-|size compared to its parent

    relative 1.0 == Length "100%" --True
-}
relative: Float ->Size
relative=
  f0To1AsPercent>> Size

f0To1AsPercent: Float ->String
f0To1AsPercent f0To1=
  (String.fromFloat ((*) 100 f0To1))
  ++"%"

absolute: Float ->Size
absolute=
  String.fromFloat >>Size


height: Size ->Svg.Attribute msg
height (Size size)=
  SvgAttr.height size

width: Size ->Svg.Attribute msg
width (Size size)=
  SvgAttr.width size


translate: Translate ->Svg.Attribute msg
translate off=
  SvgAttr.transform
    (String.concat
        [ "translate",
          "(", off|>commaSeparated, ")"
        ]
    )

points: List Translate ->Svg.Attribute msg
points positions=
  SvgAttr.points
    (positions
    |>List.map commaSeparated
    |>String.join " "
    )

commaSeparated: Translate ->String
commaSeparated off=
  [ .x off, .y off ]
  |>List.map String.fromFloat
  |>String.join ","

polygon:
    List Translate ->List (Svg.Attribute msg)
  ->Svg msg
polygon pts attrs=
  Svg.polygon
    ((points pts)::attrs)
    []

circle:
    Float ->List (Svg.Attribute msg)
  ->Svg msg
circle radius attrs=
  Svg.circle
    ( ( SvgAttr.r
          (String.fromFloat radius)
      )
      ::attrs
    )
    []


fontSize: Size ->Svg.Attribute msg
fontSize (Size size)=
  SvgAttr.fontSize size

fontFamily: String ->Svg.Attribute msg
fontFamily font=
  SvgAttr.fontFamily font



type Color=
  Color String

transparent: Color
transparent=
  Color "transparent"

feedRgb: List String ->Color
feedRgb components=
  String.concat
    [ "rgb("
    , components|>String.join ","
    , ")"
    ]
  |>Color
rgb: Float ->Float ->Float ->Color
rgb r g b=
  componentsRgb r g b |>feedRgb

componentsRgb: Float ->Float ->Float ->List String
componentsRgb red green blue=
  [ red, green, blue ]
  |>List.map f0To1AsPercent


feedRgba: List String ->Float ->Color
feedRgba components alpha=
  String.concat
    [ "rgba("
    , components|>String.join ","
    , ","
    , String.fromFloat alpha
    , ")"
    ]
  |>Color
rgba: Float ->Float ->Float ->Float ->Color
rgba r g b=
  componentsRgb r g b |>feedRgba

fillColor: Color ->Svg.Attribute msg
fillColor (Color c)=
  SvgAttr.fill c

lineColor: Color ->Svg.Attribute msg
lineColor (Color c)=
  SvgAttr.stroke c


line:
    List Translate ->List (Svg.Attribute msg)
  ->Svg msg
line pts attrs=
  Svg.polyline
    ((points pts)
      ::(fillColor transparent)
      ::attrs
    )
    []

type Linecap=
  Linecap String

type Linejoin=
  Linejoin String

roundCap: Linecap
roundCap= Linecap "round"

roundJoin: Linejoin
roundJoin= Linejoin "round"

linecap: Linecap ->Svg.Attribute msg
linecap (Linecap cap)=
  SvgAttr.strokeLinecap cap

linejoin: Linejoin ->Svg.Attribute msg
linejoin (Linejoin join)=
  SvgAttr.strokeLinejoin join

lineWidth: Size ->Svg.Attribute msg
lineWidth (Size size)=
  SvgAttr.strokeWidth size


aproximateMonospacedWidth: Float ->String ->Float
aproximateMonospacedWidth size content=
  (*)
    ((*) 0.6 size)
    (toFloat (String.length content))

computeHeight: Float ->Float
computeHeight size=
  (*) 0.6 size


type Cursor=
  Cursor String

hand: Cursor
hand= Cursor "move"
finger: Cursor
finger= Cursor "pointer"
arrow: Cursor
arrow= Cursor "default"

cursor: Cursor ->Svg.Attribute msg
cursor (Cursor kind)=
  SvgAttr.cursor kind

group:
    List (Svg.Attribute msg) ->List (Svg msg)
  ->Svg msg
group=
  Svg.g


mouseMoves: (Translate ->msg) ->Svg.Attribute msg
mouseMoves msg=
  Svg.Events.on "mousemove"
    (Json.Decode.map
      msg decodeMouseMove
    )

decodeMouseMove: Json.Decode.Decoder Translate
decodeMouseMove=
  Json.Decode.map2 xy
    (Json.Decode.field
      "movementX" Json.Decode.float
    )
    (Json.Decode.field
      "movementY" Json.Decode.float
    )

