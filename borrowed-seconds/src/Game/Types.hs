-- | The vocabulary of the game. Plain data only: no Gloss, no IO, no wall clock.
--
-- World coordinates are the design's 1280 x 720 canvas with y pointing DOWN, so the
-- numbers here read the same as the level plan. Only the renderer flips them.
module Game.Types
  ( Vec
  , Rect (..)
  , DoorId (..)
  , DoorTimers (..)
  , Occupancy (..)
  , GamePhase (..)
  , PlayerState (..)
  , TickInput (..)
  , noInput
  , Level (..)
  , GameState (..)
  , ticksPerSecond
  , timerOf
  , occupiedBy
  , doorRect
  , allDoors
  ) where

type Vec = (Double, Double)

-- | An axis-aligned rectangle. Top is the smaller y.
data Rect = Rect
  { rLeft   :: !Double
  , rTop    :: !Double
  , rRight  :: !Double
  , rBottom :: !Double
  } deriving (Eq, Show)

data DoorId = DoorA | DoorB
  deriving (Eq, Ord, Show, Enum, Bounded)

allDoors :: [DoorId]
allDoors = [minBound .. maxBound]

-- | Remaining opening time of each door, in whole ticks. Positive means open.
data DoorTimers = DoorTimers
  { timerA :: !Int
  , timerB :: !Int
  } deriving (Eq, Show)

-- | Whether the player was overlapping each doorway at the end of the last tick.
-- This is the grace that lets them walk out of a door that closes on them. It is
-- not time: nothing can read it as a duration or swap it.
data Occupancy = Occupancy
  { occupiedA :: !Bool
  , occupiedB :: !Bool
  } deriving (Eq, Show)

data GamePhase = Title | Playing | Paused | Won | Lost
  deriving (Eq, Show)

data PlayerState = PlayerState
  { psPos    :: !Vec
  , psFacing :: !Vec   -- ^ Unit vector of the last direction moved. Presentation reads it.
  , psMoving :: !Bool
  } deriving (Eq, Show)

-- | Everything the player can say to the game in one tick.
data TickInput = TickInput
  { tiUp      :: !Bool  -- ^ Held.
  , tiDown    :: !Bool  -- ^ Held.
  , tiLeft    :: !Bool  -- ^ Held.
  , tiRight   :: !Bool  -- ^ Held.
  , tiSwap    :: !Bool  -- ^ Space is down this tick. 'stepGame' finds the fresh press.
  , tiStart   :: !Bool  -- ^ One tick only: Enter was pressed.
  , tiPause   :: !Bool  -- ^ One tick only: P was pressed.
  , tiRestart :: !Bool  -- ^ One tick only: R was pressed.
  } deriving (Eq, Show)

noInput :: TickInput
noInput = TickInput False False False False False False False False

data Level = Level
  { lvStart       :: !Vec
  , lvExit        :: !Vec
  , lvExitRadius  :: !Double   -- ^ Player's centre within this of the exit wins.
  , lvCorridors   :: [Rect]    -- ^ The open floor. Only used for checks and drawing.
  , lvWalls       :: [Rect]    -- ^ Solid: what collision actually uses.
  , lvBounds      :: !Rect
  , lvDoorA       :: !Rect
  , lvDoorB       :: !Rect
  , lvTimers      :: !DoorTimers
  , lvSpeed       :: !Double   -- ^ World units per second.
  , lvRadius      :: !Double   -- ^ Player collision radius.
  } deriving (Eq, Show)

data GameState = GameState
  { gsPhase     :: !GamePhase
  , gsTimers    :: !DoorTimers
  , gsPlayer    :: !PlayerState
  , gsOccupancy :: !Occupancy
  , gsSwapHeld  :: !Bool   -- ^ Was Space down last tick? A swap needs False here.
  , gsTick      :: !Int    -- ^ Ticks played this attempt.
  , gsSwaps     :: !Int    -- ^ Exchanges made this attempt. Shown on the result screen.
  } deriving (Eq, Show)

ticksPerSecond :: Int
ticksPerSecond = 60

timerOf :: DoorId -> DoorTimers -> Int
timerOf DoorA = timerA
timerOf DoorB = timerB

occupiedBy :: DoorId -> Occupancy -> Bool
occupiedBy DoorA = occupiedA
occupiedBy DoorB = occupiedB

doorRect :: Level -> DoorId -> Rect
doorRect level DoorA = lvDoorA level
doorRect level DoorB = lvDoorB level
