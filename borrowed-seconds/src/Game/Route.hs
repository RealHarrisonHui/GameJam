-- | Scripted play, through the real rules. A plan says when to press Space; walking
-- the corridor is automatic. Running a plan yields a plain list of tick inputs, which
-- is what the tests replay and what the demo mode plays back.
module Game.Route
  ( SwapWhen (..)
  , Plan
  , intendedPlan
  , planInputs
  , runInputs
  , settle
  ) where

import Data.Foldable (foldl')
import Game.Collision (overlaps)
import Game.Level (centreline)
import Game.Logic
import Game.Types

-- | When to press Space, each entry after the one before it has fired.
data SwapWhen
  = AtTick !Int        -- ^ On this tick of the attempt or later.
  | ClearOf !DoorId    -- ^ Once the player no longer overlaps this door, beyond it.
  | InDoor !DoorId     -- ^ Once the player overlaps this door.
  deriving (Eq, Show)

type Plan = [SwapWhen]

-- | The solution: exchange at once, walk through A, and as soon as A is behind you,
-- exchange again to hand what it had left to B.
intendedPlan :: Plan
intendedPlan = [AtTick 6, ClearOf DoorA]

data Walker = Walker
  { wWaypoints :: [Vec]
  , wPlan      :: Plan
  , wGame      :: GameState
  , wInputs    :: [TickInput]  -- ^ Newest first.
  , wReleasing :: !Bool
  }

-- | Enter, then walk the centreline at full speed, pressing Space as the plan says.
-- Stops at a win, a loss, or the tick limit.
planInputs :: Level -> Int -> Plan -> [TickInput]
planInputs level limit plan =
  reverse (wInputs (go (Walker route plan started [enter] False)))
  where
    route = drop 1 centreline
    enter = noInput { tiStart = True }
    started = stepGame level enter (initialGame level)
    go w
      | gsPhase (wGame w) /= Playing = w
      | gsTick (wGame w) >= limit = w
      | otherwise = go (walk level w)

walk :: Level -> Walker -> Walker
walk level w =
  let game = wGame w
      pos = psPos (gsPlayer game)
      step = speedPerTick level
      waypoints = case wWaypoints w of
        (p : more@(_ : _)) | near step pos p -> more
        ps -> ps
      press = not (wReleasing w) && case wPlan w of
        (when' : _) -> due level game when'
        [] -> False
      plan' = if press then drop 1 (wPlan w) else wPlan w
      input = (toward step pos (headOr pos waypoints)) { tiSwap = press }
  in w
    { wWaypoints = waypoints
    , wPlan = plan'
    , wGame = stepGame level input game
    , wInputs = input : wInputs w
    , wReleasing = press
    }
  where
    headOr d [] = d
    headOr _ (p : _) = p

near :: Double -> Vec -> Vec -> Bool
near step (x, y) (tx, ty) = abs (tx - x) <= step && abs (ty - y) <= step

toward :: Double -> Vec -> Vec -> TickInput
toward step (x, y) (tx, ty) = noInput
  { tiRight = tx - x > step
  , tiLeft = x - tx > step
  , tiDown = ty - y > step
  , tiUp = y - ty > step
  }

due :: Level -> GameState -> SwapWhen -> Bool
due _ game (AtTick t) = gsTick game >= t
due level game (InDoor door) =
  overlaps (lvRadius level) (doorRect level door) (psPos (gsPlayer game))
due level game (ClearOf door) =
  pastDoor level door pos && not (overlaps (lvRadius level) (doorRect level door) pos)
  where pos = psPos (gsPlayer game)

-- | Replay tick inputs from any state.
runInputs :: Level -> GameState -> [TickInput] -> GameState
runInputs level = foldl' (flip (stepGame level))

-- | Keep ticking with no keys until something ends the attempt, or the limit.
settle :: Level -> Int -> GameState -> GameState
settle level limit g
  | limit <= 0 || gsPhase g /= Playing = g
  | otherwise = settle level (limit - 1) (stepGame level noInput g)
