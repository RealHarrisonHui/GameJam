-- | Turns key presses into game actions, one action per physical press.
--
-- Keyboards can resend "key down" while a key is held. We remember which
-- actions are held and ignore further presses until the matching release. This
-- is pure too, so it is tested in the same way as the rules.
module Game.Input
  ( App (..)
  , initialApp
  , pressAction
  , releaseAction
  ) where

import Data.List (delete)

import Game.Logic
import Game.Types

-- | Everything the running program remembers between events.
data App = App
  { appGame :: Game
  , appHeld :: [Action]  -- ^ Actions whose key is currently down.
  } deriving (Eq, Show)

initialApp :: Level -> App
initialApp level = App { appGame = initialGame level, appHeld = [] }

-- | A key went down. Restart resets the input state along with the game, so a
-- key held across a restart can never leave anything stuck.
pressAction :: Level -> Action -> App -> App
pressAction level action app
  | action == Restart         = initialApp level
  | action `elem` appHeld app = app
  | otherwise = app
      { appGame = applyAction level action (appGame app)
      , appHeld = action : appHeld app
      }

-- | A key came up, so that action can fire again on its next press.
releaseAction :: Action -> App -> App
releaseAction action app = app { appHeld = delete action (appHeld app) }
