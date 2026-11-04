-- | The clocks and the exchange tube: the heart of the presentation.
--
-- Every dial uses one scale, twelve seconds to the full circle, so that the two can be
-- compared at a glance. Colour is never the only cue: A has a round bezel studded with
-- circles and B a bezel studded with diamonds, and each dial says OPEN, CLOSING or
-- SEALED in words.
module Dial
  ( dialRadius
  , dialStatic
  , dialLive
  , tubeStatic
  , capsulesLive
  , secondsText
  , DialState (..)
  , dialStateOf
  , closingTicks
  , stateWord
  , emblem
  ) where

import Draw
import Font
import Game.Fx
import Game.Types
import Graphics.Gloss

dialRadius :: Float
dialRadius = 66

data DialState = DialOpen | DialClosing | DialSealed
  deriving (Eq)

-- | The last few seconds show on the clock face, without any flashing.
closingTicks :: Int
closingTicks = 3 * ticksPerSecond

dialStateOf :: Int -> DialState
dialStateOf t
  | t <= 0 = DialSealed
  | t <= closingTicks = DialClosing
  | otherwise = DialOpen

stateWord :: Int -> String
stateWord t = case dialStateOf t of
  DialOpen -> "OPEN"
  DialClosing -> "CLOSING"
  DialSealed -> "SEALED"

-- | Tenths, rounded up: 0.0 appears only when the door has really shut.
secondsText :: Int -> String
secondsText t =
  let tenths = (t * 10 + ticksPerSecond - 1) `div` ticksPerSecond
      (w, f) = tenths `divMod` 10
  in show w ++ "." ++ show f

-- | Each door's mark: a ring with a letter for A, a lozenge with a letter for B.
emblem :: DoorId -> Float -> Picture
emblem door r = Pictures $ case door of
  DoorA ->
    [ Color ink (ngon 24 (r + 2))
    , Color (doorColor door) (ngon 24 r)
    , Color (hex 0x2A0E08) (ngon 24 (r - 2.5))
    , letter "A"
    ]
  DoorB ->
    [ Color ink (diamond (r * 1.28 + 2.5))
    , Color (doorColor door) (diamond (r * 1.28))
    , Color (hex 0x06302C) (diamond (r * 1.28 - 3.2))
    , letter "B"
    ]
  where
    size = r * 1.05
    letter s = Color ivory (Translate 0 (negate size / 2) (textC size s))

-- | Everything about a dial that never changes, around the origin.
dialStatic :: DoorId -> Picture
dialStatic door = Pictures
  [ Translate 3 (-4) (Color (fade 0.45 ink) (ngon 48 (dialRadius + 3)))
  , Color brassLo (ngon 48 dialRadius)
  , Color brass (ngon 48 (dialRadius - 1.5))
  , Color brassHi (arcBand 8 (dialRadius - 5) (dialRadius - 3.5) 70 170)
  , Color (fade 0.7 brassLo) (arcBand 8 (dialRadius - 5) (dialRadius - 3.5) 250 350)
  , Pictures [ studAt i | i <- [0 .. 23 :: Int] ]
  , Color ink (ngon 48 (dialRadius - 8))
  , Pictures (map secondMark [0 .. 11 :: Int])
  ]
  where
    studAt i =
      let a = fromIntegral i * 15 :: Float
          r = dialRadius - 4.3
          p = (r * cos (a * pi / 180), r * sin (a * pi / 180))
      in at p $ Color brassLo $ case door of
           DoorA -> ngon 8 1.6
           DoorB -> diamond 2.2
    secondMark i =
      let a = 90 - fromIntegral i * 30 :: Float
          major = i `mod` 3 == 0
          r0 = if major then 45 else 49.5
          r1 = 55
          dirn = (cos (a * pi / 180), sin (a * pi / 180))
          on r = (fst dirn * r, snd dirn * r)
      in Color (if major then ivory else fade 0.6 ivory) (plainSeg (if major then 2.4 else 1.3) (on r0) (on r1))

-- | The moving part of a dial. Number and arc always tell the exact truth; the pulse
-- only scales the number for a moment.
dialLive :: Bool -> DoorId -> Int -> Float -> Int -> Picture
dialLive reduced door ticks pop clock = Pictures $
  [ Color (hex 0x0E1F23) (ngon 40 54) ]   -- the track the arc runs on
  ++ [ timeArc | ticks > 0 ]
  ++ [ Color face (ngon 40 42) ]
  ++ [ hand | ticks > 0 ]
  ++ [ Color (mix 0.3 iron ironHi) (plainSeg 3 (-29, 29) (29, -29)) | sealed ]
  ++ [ Color (mix 0.3 iron ironHi) (plainSeg 3 (-29, -29) (29, 29)) | sealed ]
  ++ [ Color (fade 0.25 ink) (ngon 40 30) | sealed ]
  ++ [ popRing | not reduced && pop > 0 ]
  ++ [ Translate 0 25 (Scale 0.72 0.72 (emblem door 11))
     , Translate 0 (-11) (Scale s s number)
     , Translate 0 (-28) (Color wordColour (textC 9 (stateWord ticks)))
     ]
  where
    sealed = ticks <= 0
    closing = dialStateOf ticks == DialClosing
    frac = min 1 (fromIntegral ticks / 720) :: Float
    extent = 360 * frac
    segments = max 2 (ceiling (extent / 9)) :: Int
    -- A slow breath in the last seconds, never a flash.
    breath = if closing && not reduced
      then 0.5 + 0.5 * sin (fromIntegral clock * 2 * pi / 90) else 0
    face
      | sealed = iron
      | closing = mix (0.55 + 0.25 * breath) (hex 0x101C1F) ember
      | otherwise = hex 0x0F1C1F
    arcColour = if closing then doorLight door else doorColor door
    timeArc = Color arcColour (arcBand segments 43.5 53.5 (90 - extent) 90)
    tip = (90 - extent) * pi / 180
    hand = Color ivory (plainSeg 2.5 (41 * cos tip, 41 * sin tip) (57 * cos tip, 57 * sin tip))
    number = Color (if sealed then fade 0.55 ivory else ivory) (numeralsC 23 2.9 (secondsText ticks))
    s = 1 + 0.16 * pop * pop
    wordColour
      | sealed = fade 0.75 ivory
      | closing = doorLight door
      | otherwise = doorLight door
    popRing = Color (fade (0.75 * pop) (doorLight door))
                (band 32 (dialRadius - 2 + (1 - pop) * 16) (dialRadius + 1 + (1 - pop) * 18))

-- | The twin pneumatic tube, in world coordinates, between dial centres at height @y@.
-- The upper lane runs towards the right-hand dial (A) and the lower towards the left (B).
tubeStatic :: Float -> Float -> Float -> Picture
tubeStatic bx ax y = Pictures
  [ lane (y - laneGap)
  , lane (y + laneGap)
  , Pictures [ collar x | x <- [x0, x0 + span' * 0.25, x0 + span' * 0.5, x0 + span' * 0.75, x1] ]
  , Pictures [ arrowR (x0 + span' * k) (y - laneGap) | k <- [0.12, 0.38, 0.62, 0.88] ]
  , Pictures [ arrowL (x0 + span' * k) (y + laneGap) | k <- [0.12, 0.38, 0.62, 0.88] ]
  ]
  where
    x0 = bx + dialRadius - 3
    x1 = ax - dialRadius + 3
    span' = x1 - x0
    lane ly = Pictures
      [ Color brassLo (wRect x0 (ly - 8) x1 (ly + 8))
      , Color (hex 0x0D1D20) (wRect x0 (ly - 6) x1 (ly + 6))
      , Color (fade 0.16 ivory) (wRect x0 (ly - 5) x1 (ly - 3))
      ]
    collar x = Pictures
      [ Color brassLo (wRect (x - 5) (y - laneGap - 11) (x + 5) (y + laneGap + 11))
      , Color brass (wRect (x - 3.5) (y - laneGap - 11) (x + 3.5) (y + laneGap + 11))
      , Color brassHi (wRect (x - 3.5) (y - laneGap - 11) (x - 2) (y + laneGap + 11))
      ]
    arrowR x ly = Color (fade 0.5 (doorColor DoorA)) $ Pictures
      [ wSeg 1.5 (x - 3, ly - 3.5) (x + 1, ly), wSeg 1.5 (x + 1, ly) (x - 3, ly + 3.5) ]
    arrowL x ly = Color (fade 0.5 (doorColor DoorB)) $ Pictures
      [ wSeg 1.5 (x + 3, ly - 3.5) (x - 1, ly), wSeg 1.5 (x - 1, ly) (x + 3, ly + 3.5) ]

laneGap :: Float
laneGap = 10

-- | Time in transit. Cosmetic: the timers changed the moment Space was pressed. Each
-- capsule wears the colour of the door receiving it and is labelled with the amount.
capsulesLive :: Bool -> Float -> Float -> Float -> [Capsule] -> Picture
capsulesLive reduced bx ax y = Pictures . map one
  where
    x0 = bx + dialRadius - 3
    x1 = ax - dialRadius + 3
    one c = Pictures
      [ capsule DoorA (capToA c) (lerp p x0 x1) (y - laneGap) (-1) (y - laneGap - 16)
      , capsule DoorB (capToB c) (lerp p x1 x0) (y + laneGap) 1 (y + laneGap + 26)
      ]
      where
        p = smooth (fromIntegral (capAge c) / 30)
        out = clamp01 (fromIntegral (capsuleLife - capAge c) / 10)
        capsule door amount x ly back labelY = Pictures $
          [ ribbon door x ly back | not reduced ]
          ++ [ Color (fade out (if amount > 0 then doorLight door else ironHi)) (wRect (x - 9) (ly - 4.5) (x + 9) (ly + 4.5))
             , Color (fade out ivory) (wRect (x - 4) (ly - 2.2) (x + 4) (ly + 0))
             , Color (fade out brassHi) (wRect (x - 10) (ly - 5) (x - 8) (ly + 5))
             , Color (fade out brassHi) (wRect (x + 8) (ly - 5) (x + 10) (ly + 5))
             , wAt (x, labelY) (Color (fade out (if amount > 0 then ivory else fade 0.6 ivory)) (textC 12 (secondsText amount)))
             ]
        -- The ribbon trails behind the capsule, toward where it came from.
        ribbon door x ly back = Pictures
          [ Color (fade (out * 0.34 * (1 - k)) (doorColor door))
              (wRect (min a b) (ly - 5 * (1 - k * 0.5)) (max a b) (ly + 5 * (1 - k * 0.5)))
          | i <- [0 .. 7 :: Int]
          , let k = fromIntegral i / 8
                len = 11 * (1 - abs (p - 0.5) * 1.2)
                a = x + back * (9 + len * fromIntegral i)
                b = a + back * len
          ]
