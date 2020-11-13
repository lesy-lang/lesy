module Use.Animate exposing (..)

import Use.Math exposing (sin0To1, floorDivRem)


pulse:
    { min: Float
    , max: Float
    , loopFrames: Int
    }
  ->Int ->Float
pulse { min, max, loopFrames } ticks=
  ((+)
    min
    ((*)
      (sin0To1 (floorDivRem ticks loopFrames))
      ((-) max min)
    )
  )

