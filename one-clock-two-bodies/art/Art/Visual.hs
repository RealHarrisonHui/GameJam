-- | Animation state, kept separate from the rules. The real game logic decides
-- what happens; this module only eases the picture towards it. Like the rules,
-- it is pure, so the same inputs always produce the same frames.
module Art.Visual
  ( Body (..)
  , Visual (..)
  , World (..)
  , demoWorld
  , playerWorld
  , loopLength
  , stepWorld
  , pressKey
  , releaseKey
  , worldFade
  , bodyOf
  , bodyTileX
  , bodyLift
  , bodySquash
  ) where

import Game.Input
import Game.Level
import Game.Logic
import Game.Types

-- | How one character looks right now.
data Body = Body
  { bodyFrom :: !Float  -- ^ Tile the current hop started from.
  , bodyTo   :: !Float  -- ^ Tile the current hop ends on.
  , bodyHop  :: !Float  -- ^ Progress through the hop, 0 to 1.
  , bodyLife :: !Float  -- ^ 0 is frozen stone, 1 is fully alive.
  }

data Visual = Visual
  { visA      :: !Body
  , visB      :: !Body
  , visPlateP :: !Float
  , visPlateQ :: !Float
  , visDoor1  :: !Float
  , visDoor2  :: !Float
  , visWin    :: !Float
  , visTick   :: !Int        -- ^ One tick for every move the clock has seen.
  , visTime   :: !Float
  , visOrb    :: !Float      -- ^ Progress of the light passing between bodies.
  , visOrbTo  :: !Character  -- ^ Who that light is travelling to.
  , visFadeIn :: !Float
  }

-- | The running program: the real game, its picture, and the demo script.
data World = World
  { worldApp    :: !App
  , worldVisual :: !Visual
  , worldClock  :: !Float
  , worldScript :: [(Float, Action)]  -- ^ Actions still to play, by time.
  , worldAuto   :: !Bool              -- ^ False once a player has pressed a key.
  }

level :: Level
level = level1

initialVisual :: Visual
initialVisual = Visual
  { visA = settled 1 1
  , visB = settled 1 0
  , visPlateP = 0
  , visPlateQ = 0
  , visDoor1 = 0
  , visDoor2 = 0
  , visWin = 0
  , visTick = 0
  , visTime = 0
  , visOrb = 2
  , visOrbTo = CharA
  , visFadeIn = 1
  }
  where settled tile = Body tile tile 1

demoWorld :: World
demoWorld = World
  { worldApp = initialApp level
  , worldVisual = initialVisual
  , worldClock = 0
  , worldScript = fst demoScript
  , worldAuto = True
  }

-- | The same scene with no demo: the player has the controls from the start.
playerWorld :: World
playerWorld = demoWorld { worldAuto = False, worldScript = [] }

-- | When the scripted demo starts again.
loopLength :: Float
loopLength = snd demoScript

-- | The level's real solution, spaced out so that it reads well on screen.
demoScript :: ([(Float, Action)], Float)
demoScript = (timed, endTime + 4.5)
  where
    steps = moves 2 ++ [Nothing] ++ moves 8 ++ [Nothing] ++ moves 9 ++ [Nothing] ++ moves 3
    moves n = replicate n (Just (Move DirRight))
    (endTime, timed) = foldl place (1.8, []) steps
    place (t, done) (Just action) = (t + 0.36, done ++ [(t, action)])
    place (t, done) Nothing       = (t + 1.9, done ++ [(t + 0.6, SwitchTime)])

-- | Move the whole picture forward by a slice of time.
stepWorld :: Float -> World -> World
stepWorld dt world
  | worldAuto world && worldClock world >= loopLength = demoWorld
  | otherwise = advance (foldl (flip playAction) world due)
  where
    clock = worldClock world + dt
    (due, later) = span ((<= clock) . fst) (worldScript world)
    playAction (_, action) = releaseKey action . applyKey action
    advance w = w
      { worldClock = clock
      , worldScript = later
      , worldVisual = stepVisual dt (appGame (worldApp w)) (worldVisual w)
      }

-- | A real key press. The first one ends the demo and hands over to the player.
pressKey :: Action -> World -> World
pressKey action world
  | worldAuto world = applyKey action demoWorld { worldAuto = False, worldScript = [] }
  | otherwise       = applyKey action world

releaseKey :: Action -> World -> World
releaseKey action world = world { worldApp = releaseAction action (worldApp world) }

applyKey :: Action -> World -> World
applyKey action world = world
  { worldApp = after
  , worldVisual = syncVisual (appGame before) (appGame after) (worldVisual world)
  }
  where
    before = worldApp world
    after = pressAction level action before

-- | Start any animation that a change in the game calls for.
syncVisual :: Game -> Game -> Visual -> Visual
syncVisual before after vis
  | restarted = initialVisual
  | otherwise = afterSwitch
  where
    restarted = after == initialGame level && before /= after
    afterMove
      | posA after /= posA before = ticked vis { visA = hopTo (posA after) (visA vis) }
      | posB after /= posB before = ticked vis { visB = hopTo (posB after) (visB vis) }
      | otherwise = vis
    ticked v = v { visTick = visTick v + 1 }
    afterSwitch
      | activeChar after /= activeChar before = afterMove { visOrb = 0, visOrbTo = activeChar after }
      | otherwise = afterMove

hopTo :: Pos -> Body -> Body
hopTo (x, _) body = body
  { bodyFrom = bodyTileX body
  , bodyTo = fromIntegral x
  , bodyHop = 0
  }

stepVisual :: Float -> Game -> Visual -> Visual
stepVisual dt game vis = vis
  { visA = stepBody CharA (visA vis)
  , visB = stepBody CharB (visB vis)
  , visPlateP = towards 5 (held PlateP) (visPlateP vis)
  , visPlateQ = towards 5 (held PlateQ) (visPlateQ vis)
  , visDoor1 = towards 2.5 (open Door1) (visDoor1 vis)
  , visDoor2 = towards 2.5 (open Door2) (visDoor2 vis)
  , visWin = towards 0.6 (if won then 1 else 0) (visWin vis)
  , visTime = visTime vis + dt
  , visOrb = min 2 (visOrb vis + dt / 0.7)
  , visFadeIn = max 0 (visFadeIn vis - dt / 0.9)
  }
  where
    won = isWon level game
    held p = if plateOccupied level game p then 1 else 0
    open d = if doorOpen level game d then 1 else 0
    towards rate target value
      | value < target = min target (value + rate * dt)
      | otherwise      = max target (value - rate * dt)
    stepBody who body = body
      { bodyHop = min 1 (bodyHop body + dt / 0.26)
      , bodyLife = towards 1.5 (if won || activeChar game == who then 1 else 0) (bodyLife body)
      }

-- | How dark the screen is: fading in after a restart, out before the demo loops.
worldFade :: World -> Float
worldFade world = max (smoothIn (visFadeIn (worldVisual world))) outro
  where
    smoothIn v = v * v
    outro
      | worldAuto world = max 0 (1 - (loopLength - worldClock world) / 0.8)
      | otherwise       = 0

bodyOf :: Character -> Visual -> Body
bodyOf CharA = visA
bodyOf CharB = visB

-- | Horizontal position in tiles, part-way through any hop.
bodyTileX :: Body -> Float
bodyTileX body = bodyFrom body + (bodyTo body - bodyFrom body) * eased
  where
    h = bodyHop body
    eased = h * h * (3 - 2 * h)

-- | Height above the ground, in pixels.
bodyLift :: Body -> Float
bodyLift body = 9 * sin (pi * bodyHop body)

-- | Stretch (positive) or squash (negative): a stretch mid-hop, a squash on
-- landing, and slow breathing while alive. A frozen body does not breathe.
bodySquash :: Float -> Body -> Float
bodySquash time body = hop + breath
  where
    h = bodyHop body
    hop
      | h >= 1    = 0
      | h > 0.8   = -0.12 * sin (pi * (h - 0.8) / 0.2)
      | otherwise = 0.10 * sin (pi * h / 0.8)
    breath = 0.028 * sin (time * 2.1) * bodyLife body
