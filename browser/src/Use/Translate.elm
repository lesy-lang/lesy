module Use.Translate exposing
  (..)

import Json.Encode
import Json.Decode


type alias Translate=
  { x: Float, y: Float }

x0y0: Translate
x0y0= Translate 0 0


combine:
  (Float ->Float ->Float) ->Translate
  ->(Translate ->Translate)
combine change by=
  map (change by.x) (change by.y)

mapXY: (Float ->Float) ->Translate ->Translate
mapXY change=
  map change change

map:
  (Float ->Float) ->(Float ->Float)
  ->Translate ->Translate
map changeX changeY off=
  { x= changeX (.x off)
  , y= changeY (.y off)
  }

mapX:
  (Float ->Float) ->Translate ->Translate
mapX change ({x} as translate)=
  {translate | x= change x }

mapY:
  (Float ->Float) ->Translate ->Translate
mapY change ({y} as translate)=
  {translate | y= change y }

rotation: Translate ->Float
rotation diff=
  atan2 (.y diff) (.x diff)

turned: Float ->Translate
turned by=
  { x= cos by, y= sin by }


type alias Translated a=
  {a | translate: Translate }

updateTranslate:
  (Translate ->Translate)
  ->Translated a ->Translated a
updateTranslate change translated=
  {translated
  | translate= change (.translate translated)
  }

encode:
  Translate ->Json.Encode.Value
encode translate=
  Json.Encode.object
    [ ( "x", Json.Encode.float (.x translate) )
    , ( "y", Json.Encode.float (.y translate) )
    ]

decode: Json.Decode.Decoder Translate
decode=
  Json.Decode.map2 Translate
    (Json.Decode.field "x" Json.Decode.float)
    (Json.Decode.field "y" Json.Decode.float)

