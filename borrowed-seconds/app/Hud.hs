-- | The ledger above the corridor: each door's time, in numbers, in words, and as a bar.
module Hud (hud) where

import Dial
import Draw
import Font
import Game.Fx
import Game.Types
import Graphics.Gloss

hud :: GameState -> Fx -> Picture
hud g fx = Pictures
  [ chip DoorA 610 (timerA timers) (realToFrac (fxPopA fx))
  , chip DoorB 950 (timerB timers) (realToFrac (fxPopB fx))
  , exchangeMark (realToFrac (max (fxPopA fx) (fxPopB fx)))
  ]
  where timers = gsTimers g

chipW, chipY0, chipY1 :: Float
chipW = 300
chipY0 = 14
chipY1 = 94

chip :: DoorId -> Float -> Int -> Float -> Picture
chip door x ticks pop = Pictures
  [ Color (hex 0x0E1B1E) (wRect x chipY0 (x + chipW) chipY1)
  , Color (mix pop (if sealed then ironHi else doorColor door) (doorLight door)) (frame (1.5 + pop) x chipY0 (x + chipW) chipY1)
  , wAt (x + 34, 54) (emblem door 17)
  , wAt (x + 62, 37) (Color (fade 0.85 ivory) (textL 10 ("DOOR " ++ doorName)))
  , wAt (x + 62, 56) (Color word (textL 11 (stateWord ticks)))
  , wAt (x + 262, 62) (Color (if sealed then fade 0.55 ivory else ivory) (Scale s s (numeralsR 32 3.6 (secondsText ticks))))
  , wAt (x + 268, 62) (Color (fade 0.7 ivory) (textL 13 "s"))
  -- The bar: same twelve-second scale as the dials.
  , Color (hex 0x1E2F31) (wRect (x + 62) 73 (x + 286) 83)
  , Color (if sealed then ironHi else barColour) (wRect (x + 62) 73 (x + 62 + 224 * frac) 83)
  , Pictures [ Color (hex 0x0E1B1E) (wRect bx 73 (bx + 1.2) 83)
             | i <- [1 .. 11 :: Int], let bx = x + 62 + 224 * fromIntegral i / 12 ]
  ]
  where
    sealed = ticks <= 0
    doorName = case door of { DoorA -> "A"; DoorB -> "B" }
    frac = min 1 (fromIntegral ticks / 720) :: Float
    s = 1 + 0.1 * pop * pop
    closing = dialStateOf ticks == DialClosing
    barColour = if closing then doorLight door else doorColor door
    word | sealed = fade 0.7 ivory
         | otherwise = doorLight door

-- | Two arrows between the chips, that brighten at each exchange.
exchangeMark :: Float -> Picture
exchangeMark pop = wAt (930, 54) $ Color (mix pop brass ivory) $ Pictures
  [ plainSeg 2 (-13, 5) (13, 5), plainSeg 2 (13, 5) (8, 10)
  , plainSeg 2 (13, -5) (-13, -5), plainSeg 2 (-13, -5) (-8, -10)
  ]
