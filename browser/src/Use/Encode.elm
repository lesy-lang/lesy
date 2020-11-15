module Use.Encode exposing (..)

import Json.Encode


encodeMaybe:
    (v ->Json.Encode.Value) ->Maybe v
  ->Json.Encode.Value
encodeMaybe encodeJust=
  Maybe.map
    encodeJust
  >>Maybe.withDefault
      Json.Encode.null


