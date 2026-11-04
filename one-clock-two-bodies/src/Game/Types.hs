-- | The vocabulary of the game. No logic here, only data.
module Game.Types
  ( Pos
  , Character (..)
  , Direction (..)
  , PlateId (..)
  , DoorId (..)
  , Action (..)
  , Phase (..)
  , Level (..)
  , Game (..)
  ) where

-- | (x, y) with (0, 0) at the top-left. x grows right, y grows down.
type Pos = (Int, Int)

data Character = CharA | CharB
  deriving (Eq, Show)

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq, Show)

data PlateId = PlateP | PlateQ
  deriving (Eq, Show)

data DoorId = Door1 | Door2
  deriving (Eq, Show)

-- | Everything the player can do. Every change to the game is one of these.
data Action
  = Move Direction
  | SwitchTime
  | Restart
  deriving (Eq, Show)

data Phase = Playing | Won
  deriving (Eq, Show)

-- | The map. It never changes during play.
data Level = Level
  { levelWidth  :: Int
  , levelHeight :: Int
  , levelWalls  :: [Pos]
  , levelPlates :: [(PlateId, Pos)]
  , levelDoors  :: [(DoorId, Pos)]
  , levelSpawnA :: Pos
  , levelSpawnB :: Pos
  , levelGoalA  :: Pos
  , levelGoalB  :: Pos
  } deriving (Eq, Show)

-- | The state that changes during play. Door and plate state are deliberately
-- missing: they are worked out from the positions, so they can never disagree
-- with where the characters actually are (see "Game.Logic").
data Game = Game
  { posA       :: Pos
  , posB       :: Pos
  , activeChar :: Character  -- ^ Who currently has time.
  , phase      :: Phase
  } deriving (Eq, Show)
