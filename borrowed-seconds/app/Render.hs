-- | Puts one frame together. Pure: game state and effects in, a picture out.
module Render
  ( Style (..)
  , render
  ) where

import Dial
import Draw
import Figures
import Game.Fx
import Game.Types
import Graphics.Gloss
import Hud
import Scene
import Screens

data Style = Full | Reduced
  deriving (Eq, Show)

-- | The canvas is scaled to fit the window, keeping its shape, with plain bars beside it.
render :: Style -> Scene -> Level -> GameState -> Fx -> (Int, Int) -> Picture
render style scene level g fx (winW, winH) =
  Scale k k (Pictures [ body, bars ])
  where
    k = min (fromIntegral winW / 1280) (fromIntegral winH / 720) :: Float
    bars = Color black $ Pictures
      [ rect (-4000) (-4000) 8000 3640, rect (-4000) 360 8000 3640
      , rect (-4000) (-4000) 3360 8000, rect 640 (-4000) 3360 8000 ]
    reduced = style == Reduced
    body = case gsPhase g of
      Title -> Pictures [ scTitle scene, titleLive reduced fx ]
      Paused -> Pictures [ world, pausedPanel fx ]
      Won -> Pictures [ world, wonPanel g fx, celebration ]
      Lost -> Pictures [ world, lostPanel level g fx ]
      Playing -> world
    celebration = if reduced then Blank else particles (fxParticles fx)
    world = worldView reduced scene level g fx

worldView :: Bool -> Scene -> Level -> GameState -> Fx -> Picture
worldView reduced scene level g fx = Pictures $
  [ scBoard scene ]
  ++ [ wAt (1078, 296) (Rotate wheelAngle (scWheel scene)) | not reduced ]
  ++ [ wAt (1078, 296) (scWheel scene) | reduced ]
  ++ [ wAt dialBCentre (dialLive reduced DoorB tb popB clock)
     , wAt dialACentre (dialLive reduced DoorA ta popA clock)
     , capsulesLive reduced (fst dialBCentre) (fst dialACentre) tubeY (fxCapsules fx)
     , lamps
     , exitLive reduced (scVault scene) clock (f2 (lvExit level))
     , door DoorA ta (fxGateA fx) (fxSealA fx)
     , door DoorB tb (fxGateB fx) (fxSealB fx)
     ]
  ++ [ trail (fxTrail fx) | not reduced ]
  ++ [ courier reduced (realToFrac (fxHeading fx)) stride bob moving (f2 (psPos player))
     , hud g fx
     ]
  ++ [ particles (fxParticles fx) | not reduced && gsPhase g /= Won ]
  where
    timers = gsTimers g
    ta = timerA timers
    tb = timerB timers
    clock = fxClock fx
    popA = realToFrac (fxPopA fx)
    popB = realToFrac (fxPopB fx)
    player = gsPlayer g
    moving = psMoving player && gsPhase g == Playing
    stride = realToFrac (fxStride fx)
    bob = sin (fromIntegral clock * 0.09) :: Float
    -- The wheel ticks once a second, with a little settle after each tick.
    wheelAngle =
      let secs = clock `div` ticksPerSecond
          part = fromIntegral (clock `mod` ticksPerSecond) / 8 :: Float
      in (fromIntegral secs + smooth part) * 6
    door d t gate seal = doorLive reduced d (doorRect level d) t
      (t <= 0 && occupiedBy d (gsOccupancy g))
      (realToFrac gate) (realToFrac seal) clock
    -- A small lit lamp on each door's tag says whether it can currently be used.
    lamps = Pictures [ lamp DoorA (798, 110) ta, lamp DoorB (378, 492) tb ]
    lamp d p t = wAt p $ Pictures
      [ Color ink (ngon 10 5)
      , Color (if t > 0 then doorLight d else iron) (ngon 10 3.8)
      ]
