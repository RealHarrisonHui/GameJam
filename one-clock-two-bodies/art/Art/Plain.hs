-- | The baseline: the same scene using only Gloss's built-in shapes, with no
-- generated images. Gradients and glows are faked with stacked layers. This
-- shows what the "Haskell + Gloss" option gives you without the painter.
module Art.Plain
  ( plainPicture
  ) where

import Graphics.Gloss

import Game.Types

import Art.Scene (rowY, slabLeft, slabRight, tileX)
import Art.Visual

-- | Convert from scene pixels (top-left origin) to Gloss's centred coordinates.
at :: Float -> Float -> Picture -> Picture
at x y = translate (x - 480) (300 - y)

plainPicture :: World -> Picture
plainPicture world = pictures
  [ skyStrips
  , at 480 232 (color (withAlpha 0.16 warm) (circleSolid 196))
  , at 480 232 (color (withAlpha 0.5 warm) (circle 196))
  , at 480 232 (rotate (fromIntegral (visTick vis) * 6) hand)
  , slabAt (rowY CharA)
  , slabAt (rowY CharB)
  , door jade 5 (rowY CharB) (visDoor1 vis)
  , door orchid 9 (rowY CharA) (visDoor2 vis)
  , plate jade 3 (rowY CharA) (visPlateP vis)
  , plate orchid 9 (rowY CharB) (visPlateQ vis)
  , goal CharA
  , goal CharB
  , character CharA
  , character CharB
  , color (withAlpha (worldFade world) black) (rectangleSolid 960 600)
  ]
  where
    vis = worldVisual world
    warm = makeColorI 255 226 196 255
    jade = makeColorI 79 232 180 255
    orchid = makeColorI 230 138 255 255

    skyStrips = pictures
      [ at 480 (y + 5) (color (skyAt (y / 600)) (rectangleSolid 960 11))
      | y <- [0, 10 .. 590]
      ]

    hand = pictures
      [ translate 0 73 (color warm (rectangleSolid 2 206))
      , color warm (circleSolid 5)
      ]

    slabAt top = pictures
      [ at ((slabLeft + slabRight) / 2) (top + 11) (color (makeColorI 30 22 48 255) (rectangleSolid (slabRight - slabLeft) 22))
      , at ((slabLeft + slabRight) / 2) top (color (makeColorI 255 207 168 255) (rectangleSolid (slabRight - slabLeft) 2))
      ]

    door colour tile top open = pictures
      [ at (tileX tile) (top - 52) (color (withAlpha (0.6 * (1 - open)) colour) (rectangleSolid (30 * (1 - open)) 104))
      , at (tileX tile - 15) (top - 52) (color colour (rectangleSolid 2 104))
      , at (tileX tile + 15) (top - 52) (color colour (rectangleSolid 2 104))
      , at (tileX tile) (top - 104) (color colour (rectangleSolid 32 2))
      ]

    plate colour tile top v = pictures
      [ at (tileX tile) (top - 2) (color (withAlpha (0.25 + 0.75 * v) colour) (rectangleSolid 40 5))
      , at (tileX tile) (top - 22) (color (withAlpha (0.2 * v) colour) (rectangleSolid 30 40))
      ]

    goal who =
      at (tileX 12) (rowY who) (color (withAlpha 0.75 (bodyColour who)) (outline who))

    outline CharA = pictures
      [translate 0 31 (rectangleWire 25 62), translate 0 77 (circle 8.5)]
    outline CharB = pictures
      [dome (line . closeLoop), translate 0 57 (circle 9.5)]

    character who =
      let body = bodyOf who vis
          life = bodyLife body
          x = tileX (bodyTileX body)
          y = rowY who - bodyLift body
          squash = bodySquash (visTime vis) body
          tone = mixColors life (1 - life) (bodyColour who) (makeColorI 111 106 132 255)
      in at x y (scale (1 - 0.55 * squash) (1 + squash) (pictures
           [ glow life (bodyColour who) (fst (bodyCentre who))
           , color tone (shape who)
           ]))

    bodyCentre CharA = (38 :: Float, 0 :: Float)
    bodyCentre CharB = (26, 0)

    glow life colour centre = translate 0 centre (pictures
      [ color (withAlpha (0.05 * life) colour) (circleSolid r)
      | r <- [24, 34, 44, 54, 64]
      ])

    shape CharA = pictures
      [ translate 0 31 (rectangleSolid 25 40)
      , translate 0 11 (circleSolid 12.5)
      , translate 0 51 (circleSolid 12.5)
      , translate 0 77 (circleSolid 8.5)
      ]
    shape CharB = pictures
      [ dome polygon
      , translate 0 57 (circleSolid 9.5)
      ]

    -- A circle with its base cut flat, built from straight edges.
    dome draw = draw
      [ (25 * cos a, max 0 (14 + 25 * sin a))
      | i <- [0 .. 40 :: Int]
      , let a = fromIntegral i * 2 * pi / 40
      ]

    closeLoop points = points ++ take 1 points

bodyColour :: Character -> Color
bodyColour CharA = makeColorI 255 176 79 255
bodyColour CharB = makeColorI 102 198 255 255

-- | The same sky colours as the painted scene, one flat strip at a time.
skyAt :: Float -> Color
skyAt t
  | t < 0.42  = blend (t / 0.42) (15, 12, 42) (44, 28, 85)
  | t < 0.72  = blend ((t - 0.42) / 0.3) (44, 28, 85) (122, 47, 106)
  | t < 0.9   = blend ((t - 0.72) / 0.18) (122, 47, 106) (211, 95, 108)
  | otherwise = blend ((t - 0.9) / 0.1) (211, 95, 108) (242, 154, 110)
  where
    blend k (r, g, b) (x, y, z) =
      makeColor (mixChannel k r x) (mixChannel k g y) (mixChannel k b z) 1
    mixChannel :: Float -> Float -> Float -> Float
    mixChannel k a c = (a + (c - a) * k) / 255
