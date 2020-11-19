module Id exposing
  ( Id, generate
  , encode, decode
  )
{-| random ids

# what ids are
@docs Id, generate
-}

import Dict exposing (Dict)

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
type alias Id=
  List Char

generate: (Id ->msg) ->Cmd msg
generate updateMsg=
  Random.generate
    updateMsg
    (Random.list
      8 Random.Char.unicode
    )

encode: Id ->Json.Encode.Value
encode=
  Json.Encode.string <<String.fromList

decode: Json.Decode.Decoder Id
decode=
  Json.Decode.map String.toList
    Json.Decode.string


