-- | A complete scripted run of the level, as literal tick input. The regression
-- test replays it through 'Game.Logic.stepGame', and @--demo@ plays it on screen.
module Game.Solution
  ( Segment (..)
  , solutionScript
  , expandScript
  , solutionInputs
  ) where

import Game.Types

data Segment = Segment
  { segTicks :: !Int
  , segX     :: !Int
  , segY     :: !Int
  , segHold  :: !Bool  -- ^ Space held throughout this segment
  } deriving (Eq, Show)

solutionScript :: [Segment]
solutionScript =
  [ Segment 41 1 0 False      -- walk to the slot door
  , Segment 4 0 0 False       -- wait for the shuttle to reach the bottom of its stroke
  , Segment 36 1 0 True       -- freeze it and cross the slot
  , Segment 48 1 1 False      -- release in the still room; drift north-east while repaying
  , Segment 45 1 0 False      -- under the north door
  , Segment 15 0 1 False      -- through the door, just behind the east-bound wheel
  , Segment 52 1 0 False      -- follow the wheel along the gallery
  , Segment 83 1 0 True       -- freeze it as it turns back; run for the alcove
  , Segment 17 0 (-1) True    -- step down into the alcove, still frozen
  , Segment 100 0 0 False     -- release; wait out the debt while the wheel screams past
  , Segment 20 0 1 False      -- back up into the gallery behind it
  , Segment 34 1 0 False      -- along to the exit door
  , Segment 40 0 (-1) False   -- down onto the exit
  ]

-- | One tick of Enter to leave the title, then the segments. A hold that begins
-- after no hold gets a press edge on its first tick only.
expandScript :: [Segment] -> [TickInput]
expandScript segments = noInput { tiConfirmPressed = True } : go False segments
  where
    go _ [] = []
    go wasHeld (Segment n x y hold : rest) =
      [ noInput { tiMoveX = x, tiMoveY = y, tiFreezeHeld = hold
                , tiFreezePressed = hold && not wasHeld && i == 0 }
      | i <- [0 .. n - 1 :: Int] ] ++ go hold rest

solutionInputs :: [TickInput]
solutionInputs = expandScript solutionScript
