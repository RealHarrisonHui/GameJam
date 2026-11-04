-- | The one level: a winding corridor, built from the plan's centreline.
module Game.Level
  ( theLevel
  , centreline
  , corridorWidth
  ) where

import Game.Types

corridorWidth :: Double
corridorWidth = 72

-- | Start, the four bends, and the exit, in walking order.
centreline :: [Vec]
centreline =
  [ (120, 180)
  , (840, 180)
  , (840, 420)
  , (240, 420)
  , (240, 600)
  , (1080, 600)
  ]

-- | The floor is each leg of the centreline, thickened by half the corridor width.
legRect :: Vec -> Vec -> Rect
legRect (x1, y1) (x2, y2) = Rect
  { rLeft = min x1 x2 - hw
  , rTop = min y1 y2 - hw
  , rRight = max x1 x2 + hw
  , rBottom = max y1 y2 + hw
  }
  where hw = corridorWidth / 2

-- | A door spans the corridor exactly. Its thickness is along the walking direction.
doorAcross :: Vec -> Rect
doorAcross (x, y) = Rect (x - 8) (y - hw) (x + 8) (y + hw)
  where hw = corridorWidth / 2

theLevel :: Level
theLevel = Level
  { lvStart = (120, 180)
  , lvExit = (1080, 600)
  , lvExitRadius = 26
  , lvCorridors = zipWith legRect centreline (drop 1 centreline)
  , lvWalls =
      [ Rect 0 0 1280 144        -- above everything: also the HUD's space
      , Rect 0 636 1280 720      -- below everything
      , Rect 0 144 84 216        -- behind the start
      , Rect 0 216 804 384       -- between the first and second horizontal legs
      , Rect 876 144 1280 456    -- east of the first vertical leg
      , Rect 0 384 204 636       -- west of the second vertical leg
      , Rect 276 456 1280 564    -- between the second and third horizontal legs
      , Rect 1116 564 1280 636   -- beyond the exit
      ]
  , lvBounds = Rect 0 0 1280 720
  , lvDoorA = doorAcross (780, 180)
  , lvDoorB = doorAcross (360, 420)
  , lvTimers = DoorTimers 120 720
  , lvSpeed = 220
  , lvRadius = 12
  }
