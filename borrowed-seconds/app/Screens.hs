-- | The screens that sit over the level: title, pause, and the two results.
module Screens
  ( titleLive
  , pausedPanel
  , wonPanel
  , lostPanel
  ) where

import Dial
import Draw
import Font
import Game.Fx
import Game.Logic (pastDoor)
import Game.Types
import Graphics.Gloss

-- | The dials of the attract loop: two seconds and twelve, trading places.
titleLive :: Bool -> Fx -> Picture
titleLive reduced fx = Pictures
  [ wAt (430, 330) (dialLive reduced DoorB tb pop clock)
  , wAt (850, 330) (dialLive reduced DoorA ta pop clock)
  , capsulesLive reduced 430 850 330 (fxCapsules fx)
  , wAt (640, 650) (Color (fade (0.84 + 0.16 * sin (fromIntegral clock * 2 * pi / 130)) gold) (textC 20 "PRESS ENTER TO BEGIN"))
  ]
  where
    clock = fxClock fx
    half = attractPeriod `div` 2
    -- Exchange happens half way through each period.
    rounds = (clock + half) `div` attractPeriod
    (ta, tb) = if even rounds then (120, 720) else (720, 120)
    sinceSwap = (clock + half) `mod` attractPeriod
    pop = if rounds > 0 then max 0 (1 - fromIntegral sinceSwap / 22) else 0

-- | Darken what is behind and bring a plate up from slightly below its resting place.
-- The failure plate rests low, so the courier and the door that stopped them stay in view.
panel :: Int -> Float -> Float -> Float -> Float -> [Picture] -> Picture
panel age shade restY w h contents = Pictures
  [ Color (fade (shade * smooth (fromIntegral age / 18)) ink) (rectC 1280 720)
  , Translate 0 (restY + (1 - easeOutBack (fromIntegral age / 26)) * (-30)) $ Pictures $
      [ Color (fade 0.55 ink) (Translate 5 (-7) (rectC w h))
      , Color (hex 0x0C1C20) (rectC w h)
      , Color brass (hollow w h 2.5)
      , Color brassLo (hollow (w - 16) (h - 16) 1)
      ] ++ contents
  ]

hollow :: Float -> Float -> Float -> Picture
hollow w h t = Pictures
  [ rect (-w / 2) (h / 2 - t) w t, rect (-w / 2) (-h / 2) w t
  , rect (-w / 2) (-h / 2) t h, rect (w / 2 - t) (-h / 2) t h ]

heading :: Float -> Float -> Color -> String -> Picture
heading y size c str =
  Translate (negate (textW size str) / 2) y (Color c (weighted size (size * 0.11) str))

line' :: Float -> Float -> Color -> String -> Picture
line' y size c str = Translate 0 y (Color c (textC size str))

prompt :: Float -> Int -> String -> Picture
prompt y clock str =
  line' y 14 (fade (0.84 + 0.16 * sin (fromIntegral clock * 2 * pi / 130)) gold) str

pausedPanel :: Fx -> Picture
pausedPanel fx = panel (fxPhaseAge fx) 0.62 0 440 168
  [ heading 22 40 brassHi "PAUSED"
  , line' (-12) 13 ivory "The clocks are holding."
  , prompt (-50) (fxClock fx) "P TO RESUME  *  R TO RESTART"
  ]

wonPanel :: GameState -> Fx -> Picture
wonPanel g fx = panel (fxPhaseAge fx - 24) 0.62 20 720 304
  [ heading 72 50 brassHi "VAULT CLEARED"
  , line' 36 19 ivory "Time left behind became time ahead."
  , stat (-160) "TIME TAKEN" (secondsText (gsTick g) ++ " s")
  , stat 0 "EXCHANGES" (show (gsSwaps g))
  , stat 160 "UNSPENT" (secondsText (timerA t + timerB t) ++ " s")
  , prompt (-112) (fxClock fx) "ENTER TO PLAY AGAIN"
  ]
  where
    t = gsTimers g
    stat x label value = Translate x 0 $ Pictures
      [ Translate 0 (-20) (Color (fade 0.8 ivory) (textC 11 label))
      , Translate 0 (-54) (Color gold (numeralsCText value))
      ]
    numeralsCText = textC 26

lostPanel :: Level -> GameState -> Fx -> Picture
lostPanel level g fx = panel (fxPhaseAge fx - 8) 0.42 (-222) 780 232
  [ heading 42 44 (doorLight DoorA) "TIME LOCKED"
  , line' 8 16 ivory first'
  , line' (-22) 16 brassHi second
  , prompt (-78) (fxClock fx) "ENTER TO TRY AGAIN"
  ]
  where
    pos = psPos (gsPlayer g)
    (first', second)
      | pastDoor level DoorA pos =
          ( "Both clocks ran out with Door B still ahead."
          , "Once through a door, its time is no use to it. Send it on." )
      | gsSwaps g == 0 =
          ( "Door A shut before you could reach it."
          , "Door B has time to spare. SPACE trades the two clocks." )
      | otherwise =
          ( "Both clocks ran out with Door A still ahead."
          , "A door's clock runs whether you need it or not. Trade early." )
