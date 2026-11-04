-- | All the rules. Pure and deterministic: the same level, starting state and tick
-- inputs always give the same result.
module Game.Logic
  ( swapTimers
  , tickTimers
  , isOpen
  , initialGame
  , startLevel
  , stepGame
  , exitReachable
  , pastDoor
  , speedPerTick
  ) where

import Data.Foldable (foldl')
import qualified Data.Set as Set
import Game.Collision
import Game.Types

-- | The whole mechanic: the new pair is the old pair in the opposite order.
swapTimers :: DoorTimers -> DoorTimers
swapTimers (DoorTimers a b) = DoorTimers b a

-- | One tick off each positive timer. Zero stays zero.
tickTimers :: DoorTimers -> DoorTimers
tickTimers (DoorTimers a b) = DoorTimers (down a) (down b)
  where down t = max 0 (t - 1)

isOpen :: DoorId -> DoorTimers -> Bool
isOpen door timers = timerOf door timers > 0

-- | The title screen. Timers wait here until the player starts.
initialGame :: Level -> GameState
initialGame level = GameState
  { gsPhase = Title
  , gsTimers = lvTimers level
  , gsPlayer = PlayerState (lvStart level) (1, 0) False
  , gsOccupancy = Occupancy False False
  , gsSwapHeld = False
  , gsTick = 0
  , gsSwaps = 0
  }

-- | The first moment of an attempt: what Enter and R both lead to.
startLevel :: Level -> GameState
startLevel level = (initialGame level) { gsPhase = Playing }

-- | Advance by one fixed tick. The order inside a playing tick is:
-- restart and pause, swap, timers, movement, victory, then whether progress is lost.
stepGame :: Level -> TickInput -> GameState -> GameState
stepGame level input g = case gsPhase g of
  Title
    | tiStart input -> begin
    | otherwise -> rest
  Paused
    | tiRestart input -> begin
    | tiPause input -> rest { gsPhase = Playing }
    | otherwise -> rest
  Playing
    | tiRestart input -> begin
    | tiPause input -> rest { gsPhase = Paused }
    | otherwise -> playTick level input g
  _ -- Won or Lost
    | tiStart input || tiRestart input -> begin
    | otherwise -> rest
  where
    -- Space is noted on every tick in every phase, so holding it through a pause,
    -- restart or screen change never counts as a fresh press.
    rest = g { gsSwapHeld = tiSwap input }
    begin = (startLevel level) { gsSwapHeld = tiSwap input }

playTick :: Level -> TickInput -> GameState -> GameState
playTick level input g =
  let fresh = tiSwap input && not (gsSwapHeld g)
      swapped = if fresh then swapTimers (gsTimers g) else gsTimers g
      timers = tickTimers swapped
      player = movePlayer level input timers (gsOccupancy g) (gsPlayer g)
      pos = psPos player
      inDoor door = overlaps (lvRadius level) (doorRect level door) pos
      occupancy = Occupancy (inDoor DoorA) (inDoor DoorB)
      next = g
        { gsTimers = timers
        , gsPlayer = player
        , gsOccupancy = occupancy
        , gsSwapHeld = tiSwap input
        , gsTick = gsTick g + 1
        , gsSwaps = gsSwaps g + (if fresh then 1 else 0)
        }
  in next { gsPhase = phaseAfter level next }

phaseAfter :: Level -> GameState -> GamePhase
phaseAfter level g
  | atExit = Won
  | timers == DoorTimers 0 0 && not (exitReachable level (gsOccupancy g) pos) = Lost
  | otherwise = Playing
  where
    timers = gsTimers g
    pos = psPos (gsPlayer g)
    atExit = distance pos (lvExit level) <= lvExitRadius level

distance :: Vec -> Vec -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))

speedPerTick :: Level -> Double
speedPerTick level = lvSpeed level / fromIntegral ticksPerSecond

movePlayer :: Level -> TickInput -> DoorTimers -> Occupancy -> PlayerState -> PlayerState
movePlayer level input timers occupancy player
  | dx == 0 && dy == 0 = player { psMoving = False }
  | otherwise = PlayerState pos' facing True
  where
    axis lo hi = (if hi then 1 else 0) - (if lo then 1 else 0) :: Double
    dx = axis (tiLeft input) (tiRight input)
    dy = axis (tiUp input) (tiDown input)
    len = sqrt (dx * dx + dy * dy)  -- so that diagonals are no faster
    facing = (dx / len, dy / len)
    step = speedPerTick level
    radius = lvRadius level
    pos' = moveCircle radius solidsAt (psPos player) (dx / len * step, dy / len * step)
    -- A closed door is solid unless the player was already in it and still is.
    solidsAt p = [ doorRect level door | door <- allDoors, closedFor door p ] ++ lvWalls level
    closedFor door p =
      not (isOpen door timers)
        && not (occupiedBy door occupancy && overlaps radius (doorRect level door) p)

-- | Has the player got beyond this door, along the walking route?
-- Only used for choosing advice on the failure screen.
pastDoor :: Level -> DoorId -> Vec -> Bool
pastDoor level door pos = case door of
  DoorA -> x > rRight (lvDoorA level) || y > rBottom (lvDoorA level)
  DoorB -> y > rBottom (lvDoorB level) || x < rLeft (lvDoorB level) && y >= rTop (lvDoorB level)
  where (x, y) = pos

-- | Can a player-sized circle still walk to the exit, with the doors as they are?
--
-- Breadth-first over a 12-unit grid of places the circle fits. All of the plan's
-- centrelines lie on this grid. A door the player is standing in is open to them,
-- as it is for movement. Only needed once both timers are zero.
exitReachable :: Level -> Occupancy -> Vec -> Bool
exitReachable level occupancy pos = search (Set.fromList seeds) seeds
  where
    grid = 12 :: Int
    radius = lvRadius level
    solids = lvWalls level
      ++ [ doorRect level door | door <- allDoors, not (occupiedBy door occupancy) ]
    (px, py) = pos
    nearest v = round (v / fromIntegral grid) :: Int
    worldOf (i, j) = (fromIntegral (i * grid), fromIntegral (j * grid)) :: Vec
    fits cell = not (any (\r -> overlaps radius r (worldOf cell)) solids)
    seeds =
      [ cell
      | i <- [nearest px - 1 .. nearest px + 1]
      , j <- [nearest py - 1 .. nearest py + 1]
      , let cell = (i, j)
      , fits cell
      , distance (worldOf cell) pos <= 1.5 * fromIntegral grid
      ]
    arrived cell = distance (worldOf cell) (lvExit level) <= lvExitRadius level
    search _ [] = False
    search seen frontier
      | any arrived frontier = True
      | otherwise =
          let (seen', next) = foldl' visit (seen, []) (concatMap neighbours frontier)
          in search seen' next
    visit (seen, acc) cell
      | Set.member cell seen || not (fits cell) = (seen, acc)
      | otherwise = (Set.insert cell seen, cell : acc)
    neighbours (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
