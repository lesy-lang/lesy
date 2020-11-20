module Use.Collection exposing (..)

import ZipList exposing (ZipList)
import Set


updateSelected:
  (a-> a) ->ZipList a ->ZipList a
updateSelected change list=
  ZipList.replace
    (change (ZipList.current list))
  <|list

first: ZipList a ->a
first=
  ZipList.goToEnd
  >>ZipList.current

smallestFreeIndex: List Int ->Int
smallestFreeIndex list=
  list
    |>Set.fromList
    |>Set.diff
        (List.range 0 ((-) (List.length list) 1)
        |>Set.fromList
        )
    |>Set.toList
  |>List.head
  |>Maybe.withDefault (List.length list)

