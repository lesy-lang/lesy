module Use.Json exposing
  (encodeNullable
  , encodeDict, decodeDict
  )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

import Dict exposing (Dict)


encodeNullable:
  (v ->Encode.Value) ->Maybe v
  ->Encode.Value
encodeNullable encodeJust=
  Maybe.map
    encodeJust
  >>Maybe.withDefault
      Encode.null


encodeDict:
  Int
  ->(comparable ->Encode.Value)
  ->(v ->Encode.Value)
  ->Dict comparable v ->Encode.Value
encodeDict readability encodeKey encodeValue=
  Encode.dict
    (Encode.encode readability <<encodeKey)
    encodeValue

decodeDict:
  Decoder comparable
  ->Decoder v
  ->Decoder (Dict comparable v)
decodeDict decodeKey decodeValue=
  Decode.dict decodeValue
  |>Decode.map Dict.toList
  |>Decode.map
      (List.filterMap
        (\(key, value)->
          Decode.decodeString
            decodeKey key
          |>Result.map (\k-> Just ( k, value ))
          |>Result.withDefault Nothing
        )
      )
  |>Decode.map Dict.fromList


