module Ids exposing
  ( Id, generateId
  , IdDict, empty
  , search, add, update, mapEach, values
  , encodeId, decodeId
  , encodeDict, decodeDict
  )

import Dict

import Random
import Random.Char
import Json.Encode
import Json.Decode


{-|**8 of 143,859** valid characters [#0]
→ `143,859 ^ 8 =~ 1.83440952 * 10^41` different possible ids

regarding that even if
10 billion users max
  define max 1 million morphs each,
  
the probability of duplicate ids is
  `( 1/ (4.28 x 10^20) ) * 10,000,000,000,000,000
  =~ 5,45134544 * 10^-26 =~ 5,5 * 10^-26`

#0 The Unicode Consortium. The Unicode Standard, Version 13.0.0,
    (Mountain View, CA: The Unicode Consortium, 2020. ISBN 978-1-936213-26-9)
    http://www.unicode.org/versions/Unicode13.0.0/
-}
type Id= Id (List Char)

generateId: (Id ->msg) ->Cmd msg
generateId updateMsg=
  Random.generate
    updateMsg
    ((Random.list
        8 Random.Char.unicode
      )
      |>Random.map Id
    )


type IdDict v=
  IdDict (Dict.Dict (List Char) v)

map:
    ( Dict.Dict (List Char) vA
    ->Dict.Dict (List Char) vB
    )
  ->IdDict vA ->IdDict vB
map changeValue (IdDict dict)=
  changeValue dict |>IdDict

empty: IdDict v
empty=
  IdDict Dict.empty

search: Id ->(IdDict v ->Maybe v)
search (Id id) (IdDict dict)=
  Dict.get id dict

add: Id ->v ->IdDict v ->IdDict v
add (Id id)=
  Dict.insert id >>map

update:
  Id ->(v ->v) ->IdDict v ->IdDict v
update (Id id)=
  Maybe.map >>Dict.update id >>map

mapEach:
  (Id ->vA ->vB) ->IdDict vA ->IdDict vB
mapEach change=
  Dict.map (change <<Id) |>map

values: IdDict v ->List v
values (IdDict dict)=
  Dict.values dict


encodeDict:
    (v ->Json.Encode.Value)
  ->IdDict v ->Json.Encode.Value
encodeDict encodeValue (IdDict dict)=
  Json.Encode.dict
    String.fromList
    encodeValue
    dict

encodeId: Id ->Json.Encode.Value
encodeId (Id id)=
  Json.Encode.string (String.fromList id)

decodeId: Json.Decode.Decoder Id
decodeId=
  Json.Decode.map (String.toList >>Id)
    Json.Decode.string

decodeDict:
    Json.Decode.Decoder v
  ->Json.Decode.Decoder (IdDict v)
decodeDict decodeValue=
  Json.Decode.keyValuePairs decodeValue
  |>Json.Decode.map
      (List.map
        (Tuple.mapFirst String.toList)
      >>Dict.fromList
      >>IdDict
      )

