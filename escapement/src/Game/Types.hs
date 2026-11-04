-- | Plain data shared by the pure core. Nothing here knows about graphics.
module Game.Types
  ( Vec (..), vAdd, vSub, vScale, vLerp, vLen, vDist
  , Rect (..)
  , Config (..), defaultConfig
  , TemporalMode (..), TimeState (..), TimeInput (..), initialTime
  , HazardTick
  , Route (..), Hazard (..), Gate (..), Zone (..), Prompt (..), Cog, Level (..), blankLevel
  , TickInput (..), noInput
  , Phase (..), Game (..)
  ) where

-- | World position or displacement, in virtual pixels (window centre is 0,0).
data Vec = Vec !Double !Double deriving (Eq, Show)

vAdd, vSub :: Vec -> Vec -> Vec
vAdd (Vec a b) (Vec c d) = Vec (a + c) (b + d)
vSub (Vec a b) (Vec c d) = Vec (a - c) (b - d)

vScale :: Double -> Vec -> Vec
vScale k (Vec a b) = Vec (k * a) (k * b)

vLerp :: Double -> Vec -> Vec -> Vec
vLerp u p q = vAdd p (vScale u (vSub q p))

vLen :: Vec -> Double
vLen (Vec a b) = sqrt (a * a + b * b)

vDist :: Vec -> Vec -> Double
vDist p q = vLen (vSub p q)

-- | Axis-aligned rectangle: left, bottom, right, top.
data Rect = Rect !Double !Double !Double !Double deriving (Eq, Show)

-- | Every tunable in one place.
data Config = Config
  { cfgTicksPerSecond :: !Int
  , cfgMaxDebt        :: !Int     -- ^ ticks (180 = 3 seconds)
  , cfgPlayerSpeed    :: !Double  -- ^ pixels per simulation tick
  , cfgPlayerRadius   :: !Double
  , cfgHazardRadius   :: !Double  -- ^ also the drawn tooth-tip radius
  , cfgContactGrace   :: !Double  -- ^ subtracted from the summed radii, so grazes forgive
  , cfgShuttleOneWay  :: !Int     -- ^ hazard ticks for the first wheel to cross its slot
  , cfgShuttlePhase   :: !Int
  , cfgGalleryOneWay  :: !Int     -- ^ hazard ticks for the second wheel to run its gallery
  , cfgGalleryPhase   :: !Int
  , cfgMaxFrameTime   :: !Double  -- ^ seconds; longer frames are clamped
  , cfgMaxCatchUp     :: !Int     -- ^ most simulation ticks run in one frame
  , cfgDoorGrace      :: !Double  -- ^ a closing door only crushes a player this far inside it
  , cfgPinReach       :: !Double  -- ^ how close the player's centre must come to take a pin
  , cfgFreezeBuffer   :: !Int     -- ^ a press this many ticks before the debt clears still counts
  , cfgReducedEffects :: !Bool    -- ^ drop trails, sparks and moving decoration
  , cfgGhost          :: !Bool    -- ^ show where each wheel lands when the debt clears
  , cfgHints          :: !Bool    -- ^ show teaching prompts and after-death hints
  , cfgTrailLength    :: !Int     -- ^ samples kept per hazard
  , cfgMaxParticles   :: !Int
  , cfgMaxRings       :: !Int
  , cfgEaseRate       :: !Double  -- ^ per-tick approach factor for visual tweens
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { cfgTicksPerSecond = 60
  , cfgMaxDebt        = 180
  , cfgPlayerSpeed    = 3.25
  , cfgPlayerRadius   = 13
  , cfgHazardRadius   = 28
  , cfgContactGrace   = 3
  , cfgShuttleOneWay  = 15
  , cfgShuttlePhase   = 0
  , cfgGalleryOneWay  = 90
  , cfgGalleryPhase   = 40
  , cfgMaxFrameTime   = 0.1
  , cfgMaxCatchUp     = 5
  , cfgDoorGrace      = 4
  , cfgPinReach       = 28
  , cfgFreezeBuffer   = 8
  , cfgReducedEffects = False
  , cfgGhost          = True
  , cfgHints          = True
  , cfgTrailLength    = 12
  , cfgMaxParticles   = 48
  , cfgMaxRings       = 4
  , cfgEaseRate       = 0.14
  }

data TemporalMode = Normal | Frozen | Repaying deriving (Eq, Show)

-- | Debt and the one shared hazard clock are integer ticks, so a
-- freeze-and-repay cycle balances exactly.
data TimeState = TimeState
  { tsMode  :: !TemporalMode
  , tsDebt  :: !Int
  , tsClock :: !HazardTick
  } deriving (Eq, Show)

initialTime :: TimeState
initialTime = TimeState Normal 0 0

-- | 'tiPressed' is the key-down edge, true for one tick only. Keeping it apart
-- from 'tiHeld' is what stops a held key re-freezing after a forced release.
data TimeInput = TimeInput
  { tiHeld    :: !Bool
  , tiPressed :: !Bool
  } deriving (Eq, Show)

type HazardTick = Int

-- | A polyline travelled there and back at constant speed.
data Route = Route
  { routePoints :: [Vec]
  , routeOneWay :: !Int  -- ^ hazard ticks from first point to last
  , routePhase  :: !Int  -- ^ hazard ticks already elapsed at clock zero
  } deriving (Eq, Show)

newtype Hazard = Hazard { hazardRoute :: Route } deriving (Eq, Show)

-- | A door on the clock. Whether it is open depends only on the hazard clock, so
-- the brake holds it as it is and repayment swings it at double speed.
data Gate = Gate
  { gateRect    :: !Rect
  , gatePeriod  :: !Int  -- ^ hazard ticks for one full open-and-shut cycle
  , gateOpenFor :: !Int  -- ^ hazard ticks it stands open within each cycle
  , gatePhase   :: !Int  -- ^ hazard ticks already elapsed at clock zero
  } deriving (Eq, Show)

-- | Marked floor that no wheel can reach. Decoration and test data, not a rule.
data Zone = Zone
  { zoneRect  :: !Rect
  , zoneLabel :: String
  , zoneSub   :: String
  } deriving (Eq, Show)

-- | A short line of teaching shown beside the player. The first match wins.
data Prompt = Prompt
  { promptArea :: !(Maybe Rect)
  , promptMode :: !(Maybe TemporalMode)
  , promptText :: String
  } deriving (Eq, Show)

-- | Background gear: x, y, turn rate, teeth, outer radius, inner radius.
type Cog = (Double, Double, Double, Int, Double, Double)

data Level = Level
  { lvlWalls     :: [Rect]
  , lvlFloors    :: [Rect]    -- ^ walkable space, used for drawing and checks
  , lvlStart     :: !Vec
  , lvlStartArea :: !Rect
  , lvlZones     :: [Zone]
  , lvlExit      :: !Rect
  , lvlHazards   :: [Hazard]
  , lvlGates     :: [Gate]
  , lvlPins      :: [Vec]     -- ^ all must be taken, in any order, before the exit opens
  , lvlPar       :: !Int      -- ^ ticks borrowed for the top rating; 0 means unrated
  , lvlTitle     :: String    -- ^ the floor's name
  , lvlBrief     :: [String]  -- ^ the note shown before the floor
  , lvlPrompts   :: [Prompt]  -- ^ teaches a verb, never a solution
  , lvlHint      :: [String]  -- ^ appears only after repeated deaths
  , lvlNoteAt    :: !Vec      -- ^ where the floor's name is engraved
  , lvlCogs      :: [Cog]
  , lvlGhosts    :: !Bool     -- ^ landing ghosts appear once debt has been taught
  } deriving (Eq, Show)

-- | An empty arena, for levels and tests that fill in only what they need.
blankLevel :: Level
blankLevel = Level
  { lvlWalls = [], lvlFloors = [], lvlStart = Vec 0 0, lvlStartArea = Rect 0 0 0 0
  , lvlZones = [], lvlExit = Rect 9000 9000 9001 9001, lvlHazards = []
  , lvlGates = [], lvlPins = [], lvlPar = 0
  , lvlTitle = "", lvlBrief = [], lvlPrompts = [], lvlHint = []
  , lvlNoteAt = Vec (-576) 330, lvlCogs = [], lvlGhosts = True }

-- | Everything one simulation tick needs from the outside world.
data TickInput = TickInput
  { tiMoveX          :: !Int  -- ^ -1, 0 or 1
  , tiMoveY          :: !Int
  , tiFreezeHeld     :: !Bool
  , tiFreezePressed  :: !Bool
  , tiPausePressed   :: !Bool
  , tiRestartPressed :: !Bool
  , tiConfirmPressed :: !Bool
  } deriving (Eq, Show)

noInput :: TickInput
noInput = TickInput 0 0 False False False False False

data Phase = Title | Playing | Paused | Won | Lost deriving (Eq, Show)

data Game = Game
  { gPhase    :: !Phase
  , gPlayer   :: !Vec
  , gTime     :: !TimeState
  , gTicks    :: !Int        -- ^ simulation ticks of play in this attempt
  , gBorrowed :: !Int        -- ^ total ticks frozen this attempt (end-screen statistic)
  , gHitBy    :: !(Maybe Int)  -- ^ the hazard that ended the attempt; Nothing if a door did
  , gPins     :: [Int]       -- ^ pins taken so far, most recent first
  , gQueued   :: !Bool       -- ^ a freeze pressed just before the debt cleared, still held
  } deriving (Eq, Show)
