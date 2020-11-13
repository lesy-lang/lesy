module Use.Color exposing
  ( Rgb, Rgba
  , mapRed, mapGreen, mapBlue, mapAlpha
  , toRgb, toRgba, toRgba255, toRgb255
  , withAlpha
  )

{-|each `<=0` & `>=1`,
clamped on convert (like `toRgb`)
-}
type alias Rgb=
  { red: Float
  , green: Float
  , blue: Float
  }

mapRed: (Float ->Float) ->Rgb ->Rgb
mapRed change rgb=
  { rgb | red= change (.red rgb) }

mapGreen: (Float ->Float) ->Rgb ->Rgb
mapGreen change rgb=
  { rgb | green= change (.green rgb) }


mapBlue: (Float ->Float) ->Rgb ->Rgb
mapBlue change rgb=
  { rgb | blue= change (.blue rgb) }

type alias Rgba=
  { rgb: Rgb, alpha: Float }

mapAlpha:
  (Float ->Float) ->Rgba ->Rgba
mapAlpha change rgba=
  { rgba | alpha= change (.alpha rgba) }

withAlpha: Float ->Rgb ->Rgba
withAlpha alpha rgb=
  { rgb= rgb, alpha= alpha }


clamp: Float ->Float
clamp=
  min 1 >>max 0

toRgb:
    (Float ->Float ->Float ->converted)
  ->Rgb ->converted
toRgb convert {red, green, blue}=
  convert
    (clamp red) (clamp green) (clamp blue)

toRgba:
    ( Float ->Float ->Float ->Float
    ->converted
    )
  ->Rgba ->converted
toRgba convert {rgb, alpha}=
  toRgb convert rgb alpha

to255: Float ->Int
to255= (*) 255 >>round

toRgb255:
    (Int ->Int ->Int ->converted)
  ->Rgb ->converted
toRgb255 convert=
  toRgb
    (\r g b
    ->convert
        (r|>to255) (g|>to255) (b|>to255)
    )

toRgba255:
    ( Int ->Int ->Int ->Float
    ->converted
    )
  ->Rgba ->converted
toRgba255 convert {rgb, alpha}=
  toRgb255 convert rgb alpha

