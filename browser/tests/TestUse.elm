module TestUse exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Use.Json exposing
  ( encodeDict, decodeDict
  , encodeNullable
  )
import Dict exposing (Dict)
import Json.Encode
import Json.Decode
import Use.Misc exposing (smallestFreeIndex)
import Use.Misc exposing (oneAs100Percent)


--see https://package.elm-lang.org/packages/elm-explorations/test/latest
suite: Test
suite=
  describe "use"
    [ describe "json"
        [ test "decoded dict equal to encoded dict"
          <|\_->
              let dict=
                    Dict.empty
                    |>Dict.insert 1 "first"
                    |>Dict.insert 2 "second"
                    |>Dict.insert 4 "third"
              in
              decodeEncoded
                (encodeDict 3
                  Json.Encode.int
                  Json.Encode.string
                  dict
                )
                (decodeDict
                  Json.Decode.int
                  Json.Decode.string
                )
              |>orFailWithJsonError
                  (Expect.equalDicts dict)
        
        , test "decoded nullable equal to encoded nullable (← tested)"
          <|\_->
              let just= Just 986
              in
              decodeEncoded
                (encodeNullable Json.Encode.int just)
                (Json.Decode.nullable Json.Decode.int)
              |>orFailWithJsonError
                  (Expect.equal just)
        , test "decoded nullable equal to encoded nothing (← tested)"
          <|\_->
              decodeEncoded
                (encodeNullable Json.Encode.int Nothing)
                (Json.Decode.nullable Json.Decode.int)
              |>orFailWithJsonError
                  (Expect.equal Nothing)
        ]
    , describe "collection"
        [ test "smallest free index in a list 3 0 2"
          <|\_->
              let with3= [ 3, 0, 2 ]
              in
              Expect.equal 1 (smallestFreeIndex with3)
        , test "smallest free index in an empty list"
          <|\_->
              Expect.equal 0 (smallestFreeIndex [])
        ]
    , describe "miscellaneous"
        [ test "1 as 100%: negative decimals >1"
          <|\_->
              Expect.equal (oneAs100Percent -3.7) "-370%"
        ]
    ]

decodeEncoded:
  Json.Encode.Value ->Json.Decode.Decoder a
  ->Result Json.Decode.Error a
decodeEncoded encode decode=
  Json.Decode.decodeString decode
    (Json.Encode.encode 3 encode)

orFailWithJsonError:
  (res ->Expectation)
  ->Result Json.Decode.Error res
  ->Expectation
orFailWithJsonError expectationIfOk decoded=
  case decoded of
    Ok result->
      expectationIfOk result
    
    Err err->
      Expect.fail (err |>Json.Decode.errorToString)
    
