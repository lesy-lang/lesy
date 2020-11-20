module Use.Misc exposing (..)


{-|convert  0 → 1  ^=  0% → 100%  percent string

    oneAs100Percent -1.5 --"-150%"
-}
oneAs100Percent: Float ->String
oneAs100Percent f1=
  (String.fromFloat ((*) 100 f1))
  ++"%"

