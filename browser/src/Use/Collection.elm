module Use.Collection exposing (..)

import ZipList exposing (ZipList)


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

