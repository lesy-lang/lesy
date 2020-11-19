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


--see https://package.elm-lang.org/packages/elm-explorations/test/latest
suite: Test
suite=
  describe "json"
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
    
