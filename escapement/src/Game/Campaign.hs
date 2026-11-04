-- | The run through the tower: title, a note before each floor, the floor, the
-- ending. Pure, and it only wraps 'stepGame', so a floor's rules know nothing of it.
module Game.Campaign
  ( Screen (..)
  , Campaign (..)
  , newCampaign
  , campaignAt
  , currentLevel
  , stepCampaign
  , campaignMustClear
  , hintsDue
  ) where

import Game.Logic
import Game.Types

data Screen = OnTitle | OnBrief | InFloor | OnEnding deriving (Eq, Show)

data Campaign = Campaign
  { cScreen      :: !Screen
  , cIndex       :: !Int   -- ^ which floor
  , cGame        :: !Game
  , cDeaths      :: !Int   -- ^ on this floor, since arriving
  , cTotalDeaths :: !Int
  , cTotalTicks  :: !Int   -- ^ of cleared attempts
  , cBorrowed    :: !Int
  } deriving (Eq, Show)

newCampaign :: Config -> [Level] -> Campaign
newCampaign cfg levels = Campaign OnTitle 0 (initialGame cfg (head levels)) 0 0 0 0

-- | Arrive at a floor's note directly, for testing a single floor.
campaignAt :: Config -> [Level] -> Int -> Campaign
campaignAt cfg levels i =
  let index = max 0 (min (length levels - 1) i)
  in (newCampaign cfg levels) { cScreen = OnBrief, cIndex = index, cGame = initialGame cfg (levels !! index) }

currentLevel :: [Level] -> Campaign -> Level
currentLevel levels c = levels !! cIndex c

-- | Hints wait until the player has failed here twice. Discovery comes first.
hintsDue :: Campaign -> Bool
hintsDue c = cDeaths c >= 2

stepCampaign :: Config -> [Level] -> TickInput -> Campaign -> Campaign
stepCampaign cfg levels input c = case cScreen c of
  OnTitle
    | confirmed -> c { cScreen = OnBrief }
    | otherwise -> c
  OnBrief
    | confirmed -> c { cScreen = InFloor, cGame = newAttempt cfg level }
    | otherwise -> c
  OnEnding
    | confirmed -> newCampaign cfg levels
    | otherwise -> c
  InFloor
    | gPhase game == Won && confirmed -> nextFloor
    | otherwise ->
        let game' = stepGame cfg level input game
            died = gPhase game /= Lost && gPhase game' == Lost
        in c { cGame = game'
             , cDeaths = cDeaths c + fromEnum died
             , cTotalDeaths = cTotalDeaths c + fromEnum died }
  where
    confirmed = tiConfirmPressed input
    level = currentLevel levels c
    game = cGame c
    nextFloor =
      let banked = c { cDeaths = 0
                     , cTotalTicks = cTotalTicks c + gTicks game
                     , cBorrowed = cBorrowed c + gBorrowed game }
          next = cIndex c + 1
      in if next >= length levels
           then banked { cScreen = OnEnding }
           else banked { cScreen = OnBrief, cIndex = next, cGame = initialGame cfg (levels !! next) }

-- | Held keys are forgotten across any change of screen or floor, or a restart.
campaignMustClear :: TickInput -> Campaign -> Campaign -> Bool
campaignMustClear input before after =
  cScreen before /= cScreen after
    || cIndex before /= cIndex after
    || (cScreen before == InFloor && inputMustClear input (cGame before) (cGame after))
