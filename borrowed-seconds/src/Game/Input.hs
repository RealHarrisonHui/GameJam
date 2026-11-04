-- | Key events into one 'TickInput' per tick. Pure, so held keys, fresh presses and
-- clearing on screen changes can all be tested without a window.
module Game.Input
  ( GameKey (..)
  , Keys
  , noKeys
  , keyDown
  , keyUp
  , tickInput
  , afterTick
  , clearHeld
  ) where

import qualified Data.Set as Set
import Game.Types

data GameKey = KUp | KDown | KLeft | KRight | KSpace | KEnter | KPause | KRestart
  deriving (Eq, Ord, Show)

data Keys = Keys
  { kHeld    :: !(Set.Set GameKey)
  , kPressed :: !(Set.Set GameKey)  -- ^ Went down since the last tick was taken.
  , kIgnored :: !(Set.Set GameKey)  -- ^ Cleared while down: dead until released.
  } deriving (Eq, Show)

noKeys :: Keys
noKeys = Keys Set.empty Set.empty Set.empty

-- | A second down with no up between (an OS auto-repeat) is not a new press. Neither
-- is a key that was already down when input was last cleared.
keyDown :: GameKey -> Keys -> Keys
keyDown k keys
  | Set.member k (kHeld keys) || Set.member k (kIgnored keys) = keys
  | otherwise = keys
      { kHeld = Set.insert k (kHeld keys)
      , kPressed = Set.insert k (kPressed keys)
      }

keyUp :: GameKey -> Keys -> Keys
keyUp k keys = keys
  { kHeld = Set.delete k (kHeld keys)
  , kIgnored = Set.delete k (kIgnored keys)
  }

-- | What this tick sees. A tap that went up again before the tick still counts,
-- once, for every key.
tickInput :: Keys -> TickInput
tickInput keys = TickInput
  { tiUp = down KUp
  , tiDown = down KDown
  , tiLeft = down KLeft
  , tiRight = down KRight
  , tiSwap = down KSpace
  , tiStart = pressed KEnter
  , tiPause = pressed KPause
  , tiRestart = pressed KRestart
  }
  where
    pressed k = Set.member k (kPressed keys)
    down k = Set.member k (kHeld keys) || pressed k

-- | Forget every key that is down. Each must be released before it counts again.
clearHeld :: Keys -> Keys
clearHeld keys = Keys Set.empty Set.empty (Set.union (kIgnored keys) (kHeld keys))

-- | Call after a tick has used 'tickInput'. Presses are spent here, exactly once, and
-- held keys are forgotten whenever the attempt or the screen has changed.
afterTick :: GameState -> GameState -> Keys -> Keys
afterTick before after keys
  | screenChanged = clearHeld spent
  | otherwise = spent
  where
    spent = keys { kPressed = Set.empty }
    screenChanged =
      gsPhase before /= gsPhase after || gsTick after < gsTick before
