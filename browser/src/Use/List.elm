module Use.List exposing (..)

import ZipList exposing (ZipList)


removeHead: List a ->List a
removeHead=
  List.drop 1


updateSelected:
    (a-> a) ->ZipList a ->ZipList a
updateSelected change=
  ZipList.selectedMap
    (\isSelected->
      case isSelected of
        True-> change
        False-> identity
    )