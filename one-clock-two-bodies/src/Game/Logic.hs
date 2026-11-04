-- | The rules of the game, as pure functions.
--
-- The whole game is one idea:
--
-- > applyAction :: Level -> Action -> Game -> Game
--
-- Given the same level, state and action you always get the same new state.
-- There is no clock, no randomness and no IO, so a complete playthrough is just
-- a fold over a list of actions (see 'runActions').
module Game.Logic
  ( initialGame
  , applyAction
  , runActions
  , plateOccupied
  , doorOpen
  , isWon
  , positionOf
  , linkedPlate
  ) where

import Data.List (foldl')

import Game.Types

initialGame :: Level -> Game
initialGame level = Game
  { posA       = levelSpawnA level
  , posB       = levelSpawnB level
  , activeChar = CharA
  , phase      = Playing
  }

-- | Apply one player action. This is the only way the game state ever changes.
applyAction :: Level -> Action -> Game -> Game
applyAction level Restart _ = initialGame level
applyAction _ _ game | phase game == Won = game
applyAction _ SwitchTime game = game { activeChar = otherCharacter (activeChar game) }
applyAction level (Move direction) game
  | canEnter level game target = updatePhase level (moveActive target game)
  | otherwise                  = game
  where
    target = step direction (positionOf (activeChar game) game)

-- | Play a whole list of actions, starting from the level's initial state.
runActions :: Level -> [Action] -> Game
runActions level = foldl' (flip (applyAction level)) (initialGame level)

-- | A plate is active while either character is standing on it. It does not
-- matter which character has time: a frozen body still weighs something.
plateOccupied :: Level -> Game -> PlateId -> Bool
plateOccupied level game plate =
  any (\(i, p) -> i == plate && isOccupied game p) (levelPlates level)

-- | A door is open while its plate is held, or while someone is standing in the
-- doorway. The second case means a door can never close on a character.
doorOpen :: Level -> Game -> DoorId -> Bool
doorOpen level game door =
  plateOccupied level game (linkedPlate door)
    || any (\(i, p) -> i == door && isOccupied game p) (levelDoors level)

isWon :: Level -> Game -> Bool
isWon level game = posA game == levelGoalA level && posB game == levelGoalB level

positionOf :: Character -> Game -> Pos
positionOf CharA = posA
positionOf CharB = posB

-- | Which plate opens which door.
linkedPlate :: DoorId -> PlateId
linkedPlate Door1 = PlateP
linkedPlate Door2 = PlateQ

-- Everything below is private to this module.

otherCharacter :: Character -> Character
otherCharacter CharA = CharB
otherCharacter CharB = CharA

step :: Direction -> Pos -> Pos
step DirUp    (x, y) = (x, y - 1)
step DirDown  (x, y) = (x, y + 1)
step DirLeft  (x, y) = (x - 1, y)
step DirRight (x, y) = (x + 1, y)

isOccupied :: Game -> Pos -> Bool
isOccupied game p = p == posA game || p == posB game

-- | Can the active character step onto this tile?
canEnter :: Level -> Game -> Pos -> Bool
canEnter level game target =
  inBounds level target
    && target `notElem` levelWalls level
    && not (isOccupied game target)
    && all (doorOpen level game) doorsHere
  where
    doorsHere = [door | (door, p) <- levelDoors level, p == target]

inBounds :: Level -> Pos -> Bool
inBounds level (x, y) =
  x >= 0 && x < levelWidth level && y >= 0 && y < levelHeight level

moveActive :: Pos -> Game -> Game
moveActive target game = case activeChar game of
  CharA -> game { posA = target }
  CharB -> game { posB = target }

updatePhase :: Level -> Game -> Game
updatePhase level game
  | isWon level game = game { phase = Won }
  | otherwise        = game
