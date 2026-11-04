-- | One pure simulation tick.
module Game.Logic
  ( initialGame
  , newAttempt
  , stepGame
  , hazardHit
  , moveDelta
  , inputMustClear
  ) where

import Game.Collision
import Game.Level
import Game.TimeDebt
import Game.Types

-- | The game as first launched: title screen, untouched level.
initialGame :: Config -> Level -> Game
initialGame _ level = Game
  { gPhase    = Title
  , gPlayer   = lvlStart level
  , gTime     = initialTime
  , gTicks    = 0
  , gBorrowed = 0
  , gHitBy    = Nothing
  , gPins     = []
  , gQueued   = False
  }

-- | A fresh attempt. Restart and retry both land here.
newAttempt :: Config -> Level -> Game
newAttempt cfg level = (initialGame cfg level) { gPhase = Playing }

-- | Dispatch on the screen. A tick that changes screen simulates nothing.
stepGame :: Config -> Level -> TickInput -> Game -> Game
stepGame cfg level input game = case gPhase game of
  Title
    | tiConfirmPressed input -> newAttempt cfg level
    | otherwise              -> game
  Playing
    | tiRestartPressed input -> newAttempt cfg level
    | tiPausePressed input   -> game { gPhase = Paused }
    | otherwise              -> simulate cfg level input game
  Paused
    | tiRestartPressed input -> newAttempt cfg level
    | tiPausePressed input   -> game { gPhase = Playing, gTime = resumeFromPause (gTime game) }
    | otherwise              -> game
  _finished
    | tiRestartPressed input || tiConfirmPressed input -> newAttempt cfg level
    | otherwise              -> game

-- | Update order within a playing tick. Later steps see the results of earlier ones:
--
-- 1. Temporal transition: input and debt give the new mode and clock.
-- 2. Player movement against walls and closed doors, at a speed independent of the mode.
-- 3. Hazard motion: nothing to integrate, positions come from the new clock.
-- 4. Collision, swept over the whole tick. Contact, or a door shutting on the
--    player, means Lost.
-- 5. Pins, then the exit, only if step 4 found no contact, so a loss beats a
--    simultaneous win. The exit counts only once every pin is taken.
simulate :: Config -> Level -> TickInput -> Game -> Game
simulate cfg level input game =
  let time0 = gTime game
      held = tiFreezeHeld input
      queued = gQueued game && held
      time1 = stepTime cfg (TimeInput held (tiFreezePressed input || queued)) time0
      -- A press in the last moments of repayment is kept until the debt clears,
      -- so a slightly early press is not silently lost. Only a fresh press
      -- queues, which keeps a key held through a forced release from re-freezing.
      queued1 = held && tsMode time0 == Repaying
        && (queued || (tiFreezePressed input && tsDebt time0 <= cfgFreezeBuffer cfg))
      p0 = gPlayer game
      radius = cfgPlayerRadius cfg
      closed = closedGates level (tsClock time1)
      -- A door the player already overlaps does not block, so nobody is wedged
      -- against a door that shut on their shoulder: they can step clear of it.
      blocking = filter (not . circleHitsRect radius p0) closed
      p1 = moveWithWalls (lvlWalls level ++ blocking) radius p0 (moveDelta cfg input)
      hit = hazardHit cfg level (tsClock time0) (tsClock time1) p0 p1
      crushed = any (circleHitsRect (radius - cfgDoorGrace cfg) p1) closed
      newPins =
        [ i | (i, pin) <- zip [0 ..] (lvlPins level)
            , i `notElem` gPins game
            , vDist p1 pin <= cfgPinReach cfg ]
      pins1 = reverse newPins ++ gPins game
      unlocked = length pins1 == length (lvlPins level)
      frozenNow = if tsClock time1 == tsClock time0 then 1 else 0
      moved = game
        { gPlayer   = p1
        , gTime     = time1
        , gTicks    = gTicks game + 1
        , gBorrowed = gBorrowed game + frozenNow
        , gQueued   = queued1
        }
  in case hit of
       Just index -> moved { gPhase = Lost, gHitBy = Just index }
       Nothing
         | crushed -> moved { gPhase = Lost, gHitBy = Nothing }
         | unlocked && circleHitsRect radius p1 (lvlExit level) -> moved { gPhase = Won, gPins = pins1 }
         | otherwise -> moved { gPins = pins1 }

-- | Did this tick restart or change screen? Then held keys are forgotten, so a
-- key held across the change cannot leak into the next screen.
inputMustClear :: TickInput -> Game -> Game -> Bool
inputMustClear input before after =
  gPhase before /= gPhase after || (tiRestartPressed input && gPhase before /= Title)

-- | Normalised, so diagonals are not faster.
moveDelta :: Config -> TickInput -> Vec
moveDelta cfg input =
  let x = fromIntegral (signum (tiMoveX input))
      y = fromIntegral (signum (tiMoveY input))
      len = sqrt (x * x + y * y)
  in if len == 0 then Vec 0 0 else vScale (cfgPlayerSpeed cfg / len) (Vec x y)

-- | First hazard the player touches while the clock runs from @c0@ to @c1@ and
-- the player moves from @p0@ to @p1@. Each clock tick is one linear substep, and
-- 'sweptContact' covers the relative motion inside it, so at 2x a hazard cannot
-- pass through the player between samples.
hazardHit :: Config -> Level -> HazardTick -> HazardTick -> Vec -> Vec -> Maybe Int
hazardHit cfg level c0 c1 p0 p1 =
  let steps = max 1 (c1 - c0)
      reach = cfgPlayerRadius cfg + cfgHazardRadius cfg - cfgContactGrace cfg
      playerAt k = vLerp (fromIntegral k / fromIntegral steps) p0 p1
      touches route k =
        sweptContact reach (playerAt (k - 1)) (playerAt k)
          (routePosition route (min c1 (c0 + k - 1)))
          (routePosition route (min c1 (c0 + k)))
      hits = [ i | (i, h) <- zip [0 ..] (lvlHazards level)
                 , any (touches (hazardRoute h)) [1 .. steps] ]
  in case hits of
       (i : _) -> Just i
       []      -> Nothing
