-- | Pure-logic tests. No test framework: each test is a name and a Bool.
module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Game.Input
import Game.Level
import Game.Logic
import Game.Types

-- Shorthand for writing action sequences.
right, left, up, tab :: Action
right = Move DirRight
left  = Move DirLeft
up    = Move DirUp
tab   = SwitchTime

play :: [Action] -> Game
play = runActions level1

-- | The intended solution from the design brief.
solution :: [Action]
solution =
     replicate 2 right  -- A onto plate P
  ++ [tab]
  ++ replicate 8 right  -- B through door 1, onto plate Q
  ++ [tab]
  ++ replicate 9 right  -- A through door 2, onto goal a
  ++ [tab]
  ++ replicate 3 right  -- B onto goal b

-- | No walls, so the map edge is the only thing stopping A. B sits next to A.
openLevel :: Level
openLevel = parseLevel
  [ "AB..."
  , "....."
  , "ab..."
  ]

-- | B can reach plate P here, which level1's layout does not allow.
bPlateLevel :: Level
bPlateLevel = parseLevel
  [ "AabBP1."
  ]

-- | Press and release one key.
tap :: Action -> App -> App
tap action = releaseAction action . pressAction level1 action

tests :: [(String, Bool)]
tests =
  -- The 13 required tests, in the order of the brief.
  [ ( "01 moving A does not move B"
    , let g = play [right] in posA g == (2, 2) && posB g == (1, 4) )

  , ( "02 after switching, movement affects B instead of A"
    , let g = play [tab, right] in posA g == (1, 2) && posB g == (2, 4) )

  , ( "03 switching alone preserves both positions"
    , let before = play [right]
          after  = play [right, tab]
      in posA after == posA before
           && posB after == posB before
           && activeChar after == CharB )

  , ( "04 the inactive character keeps holding its plate"
    , let g = play (replicate 2 right ++ [tab] ++ replicate 3 right)
      in activeChar g == CharB
           && posA g == (3, 2)
           && plateOccupied level1 g PlateP )

  , ( "05 a closed door blocks movement"
    , posB (play (tab : replicate 8 right)) == (4, 4) )

  , ( "06 occupying the correct plate opens only its linked door"
    , let closed = play []
          open   = play (replicate 2 right)
      in not (doorOpen level1 closed Door1)
           && doorOpen level1 open Door1
           && not (doorOpen level1 open Door2) )

  , ( "07 releasing a plate closes an unoccupied linked door"
    , let g = play (replicate 3 right)
      in not (plateOccupied level1 g PlateP) && not (doorOpen level1 g Door1) )

  , ( "08 a character inside a door can leave safely, either way"
    , let -- A holds P while B walks into door 1, then A steps off the plate.
          inDoor = play (  replicate 2 right ++ [tab] ++ replicate 4 right
                        ++ [tab, right, tab] )
          leftForward  = applyAction level1 right inDoor
          leftBackward = applyAction level1 left inDoor
      in posB inDoor == (5, 4)
           && not (plateOccupied level1 inDoor PlateP)
           && doorOpen level1 inDoor Door1
           && posB leftForward == (6, 4)
           && posB leftBackward == (4, 4)
           && not (doorOpen level1 leftForward Door1) )

  , ( "09 walls and map boundaries block movement"
    , let startOpen = initialGame openLevel
      in posA (play [up]) == (1, 2)
           && posA (play [left]) == (1, 2)
           && posA (applyAction openLevel left startOpen) == (0, 0)
           && posA (applyAction openLevel up startOpen) == (0, 0) )

  , ( "10 restart restores the exact initial gameplay state"
    , let initial = initialGame level1
      in applyAction level1 Restart (play (replicate 5 right ++ [tab, right])) == initial
           && applyAction level1 Restart (play solution) == initial )

  , ( "11 one character reaching its goal is not enough to win"
    , let aOnly = play (take 21 solution)  -- A on its goal, B still on plate Q
          bOnly = play (replicate 2 right ++ [tab] ++ replicate 11 right)  -- A still on plate P
      in posA aOnly == levelGoalA level1
           && posB aOnly /= levelGoalB level1
           && not (isWon level1 aOnly)
           && phase aOnly == Playing
           && posB bOnly == levelGoalB level1
           && posA bOnly /= levelGoalA level1
           && not (isWon level1 bOnly)
           && phase bOnly == Playing )

  , ( "12 the complete intended solution reaches victory"
    , let g = play solution
      in isWon level1 g
           && phase g == Won
           && posA g == (12, 2)
           && posB g == (12, 4) )

  , ( "13 replaying the same actions produces the same state"
    , play solution == play solution
        && play (take 11 solution) == play (take 11 solution) )

  -- Extra rules from the brief that the 13 above do not reach.
  , ( "14 level1 matches the coordinates in the brief"
    , levelWidth level1 == 15
        && levelHeight level1 == 7
        && levelSpawnA level1 == (1, 2)
        && levelSpawnB level1 == (1, 4)
        && levelPlates level1 == [(PlateP, (3, 2)), (PlateQ, (9, 4))]
        && levelDoors level1 == [(Door1, (5, 4)), (Door2, (9, 2))]
        && levelGoalA level1 == (12, 2)
        && levelGoalB level1 == (12, 4) )

  , ( "15 characters cannot occupy the same tile"
    , let g = applyAction openLevel right (initialGame openLevel)
      in posA g == (0, 0) && posB g == (1, 0) )

  , ( "16 either character can activate a plate"
    , let g = runActions bPlateLevel [tab, right]
      in posB g == (4, 0)
           && plateOccupied bPlateLevel g PlateP
           && doorOpen bPlateLevel g Door1 )

  , ( "17 after victory, movement and switching do nothing"
    , let won = play solution
      in applyAction level1 left won == won && applyAction level1 tab won == won )

  , ( "18 input: a repeated key-down without key-up moves only once"
    , let app = pressAction level1 right
                  (pressAction level1 right (pressAction level1 right (initialApp level1)))
      in posA (appGame app) == (2, 2) )

  , ( "19 input: after key-up the same key moves again"
    , posA (appGame (tap right (tap right (initialApp level1)))) == (3, 2) )

  , ( "20 input: a repeated Tab key-down switches only once"
    , let app = pressAction level1 tab (pressAction level1 tab (initialApp level1))
      in activeChar (appGame app) == CharB )

  , ( "21 input: restart clears held keys and the game, even after victory"
    , let held = pressAction level1 right (foldr tap (initialApp level1) (reverse solution))
          app  = pressAction level1 Restart held
      in phase (appGame held) == Won
           && app == initialApp level1
           && posA (appGame (pressAction level1 right app)) == (2, 2) )
  ]

main :: IO ()
main = do
  mapM_ report tests
  let failures = length (filter (not . snd) tests)
  putStrLn (show (length tests - failures) ++ " of " ++ show (length tests) ++ " tests passed")
  unless (failures == 0) exitFailure
  where
    report (name, passed) =
      putStrLn ((if passed then "PASS  " else "FAIL  ") ++ name)
