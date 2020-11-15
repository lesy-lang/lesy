module Use.Math exposing (..)

import Use.Translate exposing (..)


floorDivRem: Int ->Int ->Float
floorDivRem a b=
  (/)
    (toFloat (modBy b a))
    (toFloat b)

sin0To1: Float ->Float
sin0To1 turns=
  (/)
    ((+) 
      1 (sin (Basics.turns turns))
    )
    2

equilateralTriangleHeight: Float ->Float
equilateralTriangleHeight side=
  (*) side ((/) (sqrt 3) 2)

regularPoly: Int ->Float ->Float ->List Translate
regularPoly edges toMid turn=
  let anglePerEdge=
        (/) ((*) 2 pi) (toFloat edges)
  in
  List.range 0 ((-) edges 1)
  |>List.map
      (toFloat >>((*) anglePerEdge)
      >>((+) turn)
      >>turned
      >>mapXY ((*) toMid)
      )

