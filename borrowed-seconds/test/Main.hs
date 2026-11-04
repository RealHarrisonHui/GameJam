-- | Plain assertions over the pure core. No test framework, no graphics.
-- Route checks all go through 'stepGame', never around it.
module Main (main) where

import Control.Monad (unless)
import Data.Foldable (foldl')
import Data.IORef
import Data.List (findIndex, scanl')
import Game.Clock
import Game.Collision
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Route
import Game.Types
import System.Exit (exitFailure)

level :: Level
level = theLevel

-- A test is a name and a result, with details for when it fails.
data T = T String Bool String

check :: String -> Bool -> T
check name ok = T name ok ""

checkEq :: (Eq a, Show a) => String -> a -> a -> T
checkEq name expected actual =
  T name (expected == actual) ("expected " ++ show expected ++ "\n      actual " ++ show actual)

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  count <- newIORef (0 :: Int)
  let section title ts = do
        putStrLn title
        mapM_ report ts
      report (T name ok detail) = do
        modifyIORef' count (+ 1)
        putStrLn ("  " ++ (if ok then "ok    " else "FAIL  ") ++ name)
        unless (ok || null detail) (putStrLn ("      " ++ detail))
        unless ok (modifyIORef' failures (+ 1))
  section "Timer exchange (1-6)" exchangeTests
  section "Fresh presses (7)" holdTests
  section "Doors (8-9)" doorTests
  section "Walls (10)" wallTests
  section "Pause (11)" pauseTests
  section "Restart (12)" restartTests
  section "Failure and zero timers (13)" zeroTests
  section "Route through real gameplay (14)" routeTests
  section "Determinism (15)" determinismTests
  section "Flow" flowTests
  section "Input adapter" inputTests
  section "Fixed-step clock" clockTests
  section "Effects" fxTests
  reportRoute
  n <- readIORef count
  bad <- readIORef failures
  putStrLn (show (n - bad) ++ " passed, " ++ show bad ++ " failed")
  unless (bad == 0) exitFailure

-- Building blocks ---------------------------------------------------------------

playing :: GameState
playing = startLevel level

-- | A playing state with chosen timers and player position, as if arrived at by
-- walking: occupancy is what it would be at that spot.
placed :: DoorTimers -> Vec -> GameState
placed timers pos = playing
  { gsTimers = timers
  , gsPlayer = PlayerState pos (1, 0) False
  , gsOccupancy = Occupancy (inDoor DoorA pos) (inDoor DoorB pos)
  }

inDoor :: DoorId -> Vec -> Bool
inDoor door = overlaps (lvRadius level) (doorRect level door)

right, left', up', down', space :: TickInput
right = noInput { tiRight = True }
left' = noInput { tiLeft = True }
up' = noInput { tiUp = True }
down' = noInput { tiDown = True }
space = noInput { tiSwap = True }

run :: Int -> TickInput -> GameState -> GameState
run n input g = runInputs level g (replicate n input)

posOf :: GameState -> Vec
posOf = psPos . gsPlayer

xOf, yOf :: GameState -> Double
xOf = fst . posOf
yOf = snd . posOf

-- | Scripted arrow keys from a small deterministic generator.
noisyInputs :: Int -> Int -> [TickInput]
noisyInputs seed n = take n (concatMap hold (iterate next (next seed)))
  where
    next s = (s * 1103515245 + 12345) `mod` 2147483648
    hold s =
      let bit k = (s `div` (2 ^ (k :: Int))) `mod` 2 == 1
          ticks = 1 + (s `div` 65536) `mod` 40
          input = noInput
            { tiUp = bit 8, tiDown = bit 9, tiLeft = bit 10, tiRight = bit 11 || bit 12 }
      in replicate ticks input

-- 1-6 ---------------------------------------------------------------------------

exchangeTests :: [T]
exchangeTests =
  [ checkEq "1. swap exchanges the exact values" (DoorTimers 540 60) (swapTimers (DoorTimers 60 540))
  , check "2. swap preserves the sum" $
      and [ sumOf (swapTimers t) == sumOf t | t <- samples ]
  , check "3. swapping twice restores the pair" $
      and [ swapTimers (swapTimers t) == t | t <- samples ]
  , check "4. swapping zero with positive time reopens the correct door" $
      let before = DoorTimers 0 300
          after = swapTimers before
      in not (isOpen DoorA before) && isOpen DoorA after && timerA after == 300
           && isOpen DoorB before && not (isOpen DoorB after)
  , checkEq "4b. and in play: a closed door opens that very tick"
      (DoorTimers 299 0)
      (gsTimers (stepGame level space (placed (DoorTimers 0 300) (120, 180))))
  , checkEq "5. ticking lowers positive timers only" (DoorTimers 0 4) (tickTimers (DoorTimers 0 5))
  , checkEq "5b. ticking lowers both when both are positive" (DoorTimers 9 4) (tickTimers (DoorTimers 10 5))
  , check "6. timers never go negative" $
      let DoorTimers a b = iterate tickTimers (DoorTimers 3 0) !! 50 in a == 0 && b == 0
  , checkEq "6b. in play they stop at zero" (DoorTimers 0 0)
      (gsTimers (settle level 2000 playing))
  , check "6c. no tick of a long noisy run has a negative timer" $
      all (\g -> timerA (gsTimers g) >= 0 && timerB (gsTimers g) >= 0)
        (scanl' (flip (stepGame level)) playing (noisyInputs 7 1500))
  ]
  where
    sumOf (DoorTimers a b) = a + b
    samples = [ DoorTimers a b | a <- [0, 1, 7, 120], b <- [0, 3, 720] ]

-- 7 -----------------------------------------------------------------------------

holdTests :: [T]
holdTests =
  [ checkEq "7. holding Space swaps once" 1 (gsSwaps (run 200 space playing))
  , checkEq "7b. holding Space leaves one swap's result (timers ran 200 ticks)"
      (DoorTimers 520 0) (gsTimers (run 200 space playing))
  , checkEq "7c. release and press again swaps again" 2
      (gsSwaps (runInputs level playing [space, space, noInput, space, space]))
  , check "7d. swapping on the tick timers would run out still transfers" $
      gsTimers (stepGame level space (placed (DoorTimers 1 5) (120, 180))) == DoorTimers 4 0
  ]

-- 8-9 ---------------------------------------------------------------------------

doorATouch, doorBTouch :: Double
doorATouch = rLeft (lvDoorA level) - lvRadius level
doorBTouch = rRight (lvDoorB level) + lvRadius level

doorTests :: [T]
doorTests =
  [ check "8. a closed door A stops the player, touching" $
      let g = run 300 right (placed (DoorTimers 0 900) (700, 180))
      in abs (xOf g - doorATouch) < 0.01
  , check "8b. a closed door B stops the player from the east, touching" $
      let g = run 300 left' (placed (DoorTimers 900 0) (500, 420))
      in abs (xOf g - doorBTouch) < 0.01
  , check "8c. an open door lets the player through" $
      xOf (run 60 right (placed (DoorTimers 900 900) (700, 180))) > rRight (lvDoorA level)
  , check "8d. very high speed cannot tunnel through a closed door" $
      let fast = level { lvSpeed = 500000 }
          g = runInputs fast (placed (DoorTimers 0 900) (700, 180)) [right, right, right]
      in xOf g <= doorATouch + 0.01
  , check "8e. a closed door blocks the whole corridor width, wall to wall" $
      and [ xOf (run 200 right (placed (DoorTimers 0 900) (700, y))) <= doorATouch + 0.01
          | y <- [156, 164, 172, 180, 188, 196, 204] ]
  , check "9. when a door closes on the player they can walk out ahead" $
      let g = run 30 right (placed (DoorTimers 1 400) (780, 180))
      in xOf g > rRight (lvDoorA level) + lvRadius level && timerA (gsTimers g) == 0
  , check "9b. or walk back the way they came" $
      let g = run 30 left' (placed (DoorTimers 1 400) (780, 180))
      in xOf g < rLeft (lvDoorA level) - lvRadius level
  , check "9c. the door was not moved or reopened: the player was not pushed" $
      let start = placed (DoorTimers 1 400) (780, 180)
          g = run 40 noInput start
      in posOf g == (780, 180) && timerA (gsTimers g) == 0 && occupiedA (gsOccupancy g)
  , check "9d. the grace is not time: swapping hands over nothing from it" $
      -- A has shut on the player, who is still in it. B holds 260 ticks. The swap
      -- gives A those 260 (less the tick) and gives B exactly A's nothing.
      let inDoorway = run 40 noInput (placed (DoorTimers 1 300) (780, 180))
      in occupiedA (gsOccupancy inDoorway)
           && gsTimers inDoorway == DoorTimers 0 260
           && gsTimers (stepGame level space inDoorway) == DoorTimers 259 0
  , check "9e. timers in the doorway fall exactly as elsewhere" $
      let a = gsTimers (run 40 noInput (placed (DoorTimers 10 400) (780, 180)))
          b = gsTimers (run 40 noInput (placed (DoorTimers 10 400) (500, 180)))
      in a == b
  , check "9f. once clear, the door is solid again" $
      let out = run 30 right (placed (DoorTimers 1 400) (780, 180))
          back = run 60 left' out
      in xOf back >= rRight (lvDoorA level) + lvRadius level - 0.01
           && not (occupiedA (gsOccupancy back))
  , check "9g. an exchange that empties the door you stand in also leaves you free" $
      let g0 = placed (DoorTimers 300 0) (360, 420)
          g = run 30 left' (stepGame level space g0)
      in timerB (gsTimers g0) == 0 && xOf g < rLeft (lvDoorB level) - lvRadius level
  ]

-- 10 ----------------------------------------------------------------------------

wallTests :: [T]
wallTests =
  [ check "10. walls and floor are exactly complementary, over the whole canvas" $
      and [ inAny (lvCorridors level) p /= inAny (lvWalls level) p
          | x <- [0.5, 6.5 .. 1279], y <- [0.5, 6.5 .. 717], let p = (x, y) ]
  , check "10b. every door is on the floor and spans it between two walls" $
      and [ inAny (lvCorridors level) (cx d, rTop (doorRect level d) + 1)
              && inAny (lvCorridors level) (cx d, rBottom (doorRect level d) - 1)
              && inAny (lvWalls level) (cx d, rTop (doorRect level d) - 1)
              && inAny (lvWalls level) (cx d, rBottom (doorRect level d) + 1)
          | d <- allDoors ]
  , check "10c. no shortcut down from the first leg to the second" $
      yOf (run 400 down' (placed bothOpen (400, 180))) <= 216 - lvRadius level + 0.01
  , check "10d. no shortcut up from the second leg to the first" $
      yOf (run 400 up' (placed bothOpen (400, 420))) >= 384 + lvRadius level - 0.01
  , check "10e. no shortcut down from the second leg to the third" $
      yOf (run 400 down' (placed bothOpen (600, 420))) <= 456 - lvRadius level + 0.01
  , check "10f. no shortcut east from the first vertical leg" $
      xOf (run 400 right (placed bothOpen (840, 300))) <= 876 - lvRadius level + 0.01
  , check "10g. the first leg's end cap holds" $
      xOf (run 400 left' (placed bothOpen (120, 180))) >= 84 + lvRadius level - 0.01
  , check "10h. a long noisy walk stays on the floor and never jumps" $
      let trail = scanl' (flip (stepGame level)) (placed longOpen (120, 180))
                    (concatMap (noisyInputs 3) [6000])
          positions = map posOf trail
          jump (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
      in all (\p -> not (any (\w -> overlaps (lvRadius level) w p) (lvWalls level))) positions
           && and (zipWith (\a b -> jump a b <= speedPerTick level * 1.01) positions (drop 1 positions))
  , check "10i. noisy walking with door A shut never gets past it" $
      let trail = scanl' (flip (stepGame level)) (placed (DoorTimers 0 100000) (120, 180))
                    (noisyInputs 11 6000)
      in all (\g -> xOf g <= doorATouch + 0.01) trail
  , check "10j. diagonal movement is no faster than straight" $
      let g = stepGame level (noInput { tiRight = True, tiDown = True }) (placed bothOpen (400, 180))
          (x, y) = posOf g
          d = sqrt ((x - 400) ^ (2 :: Int) + (y - 180) ^ (2 :: Int))
      in abs (d - speedPerTick level) < 1e-6
  , check "10k. sliding: diagonal into a wall still makes progress along it" $
      let g = run 60 (noInput { tiRight = True, tiUp = True }) (placed bothOpen (400, 180))
      in xOf g > 500 && yOf g >= 144 + lvRadius level - 0.01
  ]
  where
    bothOpen = DoorTimers 99999 99999
    longOpen = DoorTimers 99999 99999
    cx d = (rLeft (doorRect level d) + rRight (doorRect level d)) / 2
    inAny rects (x, y) =
      any (\(Rect l t r b) -> x >= l && x <= r && y >= t && y <= b) rects

-- 11 ----------------------------------------------------------------------------

pauseTests :: [T]
pauseTests =
  [ checkEq "11. pause stops timers and movement"
      (gsTimers moving, posOf moving, gsTick moving)
      (gsTimers paused, posOf paused, gsTick paused)
  , checkEq "11b. Space while paused does nothing" 0 (gsSwaps paused)
  , check "11c. resume carries on from the same place" $
      let g = stepGame level (noInput { tiPause = True }) paused
      in gsPhase g == Playing && gsTimers g == gsTimers moving && posOf g == posOf moving
  , checkEq "11d. Space held across a pause does not swap on resume" 0 $
      gsSwaps (runInputs level moving
        [ noInput { tiPause = True, tiSwap = True }, space, space
        , space { tiPause = True }, space, space ])
  ]
  where
    moving = run 30 right playing
    paused = runInputs level moving (noInput { tiPause = True } : replicate 200 (right { tiSwap = True }))

-- 12 ----------------------------------------------------------------------------

restartTests :: [T]
restartTests =
  [ checkEq "12. R in play restores the exact starting state" playing
      (stepGame level restart mid)
  , checkEq "12b. R while paused too" playing
      (stepGame level restart (stepGame level (noInput { tiPause = True }) mid))
  , checkEq "12c. R after losing too" playing (stepGame level restart lost)
  , checkEq "12d. Enter after losing too" playing (stepGame level (noInput { tiStart = True }) lost)
  , checkEq "12e. R after winning too" playing (stepGame level restart won)
  , checkEq "12f. Enter after winning too" playing (stepGame level (noInput { tiStart = True }) won)
  , checkEq "12g. Space held through R is not a fresh press" 0
      (gsSwaps (runInputs level mid [restart { tiSwap = True }, space, space]))
  , check "12h. R does nothing on the title screen" $
      stepGame level restart (initialGame level) == initialGame level
  ]
  where
    restart = noInput { tiRestart = True }
    mid = runInputs level playing (noisyInputs 5 200)
    lost = settle level 2000 playing
    won = runInputs level (initialGame level) (planInputs level 3000 intendedPlan)

-- 13 ----------------------------------------------------------------------------

zeroTests :: [T]
zeroTests =
  [ check "13. zero timers do not fail a player past both doors, who can then win" $
      let inputs = planInputs level 3000 intendedPlan
          states = scanl' (flip (stepGame level)) (initialGame level) inputs
          clearOfB g = gsPhase g == Playing && pastDoor level DoorB (posOf g) && not (inDoor DoorB (posOf g))
      in case findIndex clearOfB states of
           Nothing -> False
           Just i ->
             let waiting = settleZero (states !! i)
                 final = runInputs level waiting (drop i inputs)
             in gsTimers waiting == DoorTimers 0 0 && gsPhase waiting == Playing
                  && gsPhase final == Won
  , checkEq "13b. both zero with both doors ahead is a loss" Lost
      (gsPhase (settle level 2000 playing))
  , checkEq "13c. both zero between the doors is a loss" Lost
      (gsPhase (stepGame level noInput (placed (DoorTimers 1 1) (840, 300))))
  , checkEq "13d. both zero west of B, the exit still ahead, is not" Playing
      (gsPhase (stepGame level noInput (placed (DoorTimers 1 1) (240, 500))))
  , checkEq "13e. standing in door B as both hit zero, with the way on clear, is not" Playing
      (gsPhase (stepGame level noInput (placed (DoorTimers 1 1) (360, 420))))
  , checkEq "13f. and that player can still walk out west and win" Won
      (gsPhase (runInputs level (placed (DoorTimers 1 1) (360, 420))
        (replicate 40 left' ++ replicate 80 (noInput { tiLeft = True, tiDown = True })
          ++ replicate 400 right)))
  , checkEq "13g. but walking back east out of door B at zero is a loss" Lost
      (gsPhase (run 40 right (placed (DoorTimers 1 1) (360, 420))))
  , checkEq "13h. standing in door A as both hit zero, with B shut ahead, is a loss" Lost
      (gsPhase (stepGame level noInput (placed (DoorTimers 1 1) (780, 180))))
  , check "13i. the exit counts as reachable from everywhere beyond door B" $
      all (exitReachable level (Occupancy False False)) [(300, 420), (240, 500), (700, 600), (1000, 600)]
  , check "13j. and from nowhere before it" $
      not (any (exitReachable level (Occupancy False False)) [(400, 420), (840, 300), (800, 180), (200, 180)])
  ]
  where
    settleZero g
      | gsTimers g == DoorTimers 0 0 || gsPhase g /= Playing = g
      | otherwise = settleZero (stepGame level noInput g)

-- 14 ----------------------------------------------------------------------------

finish :: Plan -> GameState
finish plan = runInputs level (initialGame level) (planInputs level 3000 plan)

-- | A plan that stopped early has not necessarily ended: let the clock run it out.
outcome :: Plan -> GamePhase
outcome plan = gsPhase (settle level 3000 (finish plan))

routeTests :: [T]
routeTests =
  [ checkEq "14. the intended two-swap route wins through stepGame" Won (gsPhase intended)
  , checkEq "14b. using exactly two exchanges" 2 (gsSwaps intended)
  , check "14c. door A cannot be reached before its two seconds expire" $
      let g = run 120 right playing
      in timerA (gsTimers g) == 0 && xOf g < doorATouch
  , check "14d. so going straight there is stopped by door A" $
      let g = run 300 right playing in abs (xOf g - doorATouch) < 0.01
  , checkEq "14e. no exchange at all loses" Lost (outcome [])
  , check "14f. after the first exchange, B's short time expires before arrival" $
      let inputs = planInputs level 3000 [AtTick 6]
          g = runInputs level (initialGame level) inputs
      in gsPhase g == Lost && gsSwaps g == 1 && xOf g > rRight (lvDoorB level)
  , check "14g. crossing A with the first exchange leaves it with time to hand on" $
      let inputs = planInputs level 3000 intendedPlan
          states = scanl' (flip (stepGame level)) (initialGame level) inputs
      in case filter ((== 2) . gsSwaps) states of
           (g : _) -> timerB (gsTimers g) > ticksToDoorB && timerA (gsTimers g) == 0
                        && pastDoor level DoorA (posOf g)
           [] -> False
  , check "14h. the second exchange closes A behind the player" $
      let inputs = planInputs level 3000 intendedPlan
          states = scanl' (flip (stepGame level)) (initialGame level) inputs
      in case filter ((== 2) . gsSwaps) states of
           (g : _) -> xOf g >= rRight (lvDoorA level) + lvRadius level - 0.01
                        && not (isOpen DoorA (gsTimers g)) && isOpen DoorB (gsTimers g)
           [] -> False
  , check "14i. a second exchange before reaching A shuts A in the player's face" $
      outcome [AtTick 6, AtTick 30] == Lost
  , check "14j. no single exchange, at any moment, can win" $
      all (\t -> outcome [AtTick t] == Lost) [0, 15 .. 720]
  , check "14k. the first exchange can come any time before arriving: all of these win" $
      all (\t -> outcome [AtTick t, ClearOf DoorA] == Won) [0, 10 .. 170]
  , check "14l. exchanging on the very first playable tick also wins" $
      outcome [AtTick 0, ClearOf DoorA] == Won
  , checkEq "14n. exchanging while standing in door A shuts it, walks free, and still wins" Won
      (outcome [AtTick 6, InDoor DoorA])
  , check "14o. and at that moment the door is at zero with only the grace holding it" $
      let states = scanl' (flip (stepGame level)) (initialGame level)
                     (planInputs level 3000 [AtTick 6, InDoor DoorA])
      in case filter ((== 2) . gsSwaps) states of
           (g : _) -> timerA (gsTimers g) == 0 && occupiedA (gsOccupancy g)
           [] -> False
  , check "14m. extra exchange pairs after A, while B has plenty, still win" $
      outcome [AtTick 6, ClearOf DoorA, AtTick 200, AtTick 210] == Won
  ]
  where
    intended = finish intendedPlan
    -- 60 + 240 + 480 units from door A to door B, at full speed.
    ticksToDoorB = ceiling ((780 :: Double) / speedPerTick level) :: Int

reportRoute :: IO ()
reportRoute = do
  let inputs = planInputs level 3000 intendedPlan
      states = scanl' (flip (stepGame level)) (initialGame level) inputs
      final = last states
      afterSwap n = case filter ((== n) . gsSwaps) states of
        (g : _) -> show (gsTick g, gsTimers g, posOf g)
        [] -> "-"
  putStrLn "Intended route:"
  putStrLn ("  first exchange (tick, timers, pos):  " ++ afterSwap 1)
  putStrLn ("  second exchange (tick, timers, pos): " ++ afterSwap 2)
  putStrLn ("  end: " ++ show (gsPhase final) ++ " at tick " ++ show (gsTick final)
            ++ ", " ++ show (gsTimers final))

-- 15 ----------------------------------------------------------------------------

determinismTests :: [T]
determinismTests =
  [ check "15. the same tick inputs give the same state, at every tick" $
      let inputs = planInputs level 3000 intendedPlan ++ noisyInputs 9 1000
          runIt = scanl' (flip (stepGame level)) (initialGame level) inputs
      in runIt == scanl' (flip (stepGame level)) (initialGame level) inputs
  , checkEq "15b. the plan's recorded inputs, replayed open-loop, reach the same win"
      (Won, 2) (gsPhase (finish intendedPlan), gsSwaps (finish intendedPlan))
  , check "15c. a noisy history with swaps replays identically" $
      let history = zipWith (\i n -> i { tiSwap = n `mod` 37 < 3 }) (noisyInputs 21 2000) [0 :: Int ..]
      in runInputs level playing history == runInputs level playing history
  ]

-- Flow --------------------------------------------------------------------------

flowTests :: [T]
flowTests =
  [ checkEq "timers wait on the title screen" (initialGame level)
      (runInputs level (initialGame level) (replicate 500 right))
  , checkEq "Enter starts" Playing (gsPhase (stepGame level (noInput { tiStart = True }) (initialGame level)))
  , checkEq "the tick that starts spends no time" (lvTimers level)
      (gsTimers (stepGame level (noInput { tiStart = True }) (initialGame level)))
  , checkEq "the first tick of play spends one" (DoorTimers 119 719) (gsTimers (stepGame level noInput playing))
  , check "Enter does nothing while playing" $
      stepGame level (noInput { tiStart = True }) playing == stepGame level noInput playing
  , check "P does nothing on a result screen" $
      let lost = settle level 2000 playing
      in stepGame level (noInput { tiPause = True }) lost == lost
  , checkEq "reaching the exit wins" Won (gsPhase (stepGame level right (placed (DoorTimers 0 0) (1056, 600))))
  , check "winning stops the clock" $
      let won = finish intendedPlan in gsTimers (run 100 noInput won) == gsTimers won
  ]

-- Input adapter -----------------------------------------------------------------

inputTests :: [T]
inputTests =
  [ check "holding a key reads as held on every tick" $
      let k = keyDown KRight noKeys
      in tiRight (tickInput k) && tiRight (tickInput (spend k))
  , check "Enter, P and R count on one tick only" $
      let k = keyDown KEnter (keyDown KPause (keyDown KRestart noKeys))
          i0 = tickInput k
          i1 = tickInput (spend k)
      in tiStart i0 && tiPause i0 && tiRestart i0
           && not (tiStart i1 || tiPause i1 || tiRestart i1)
  , check "a press with no tick yet stays until one takes it" $
      tiStart (tickInput (keyDown KEnter noKeys))
  , check "a tap entirely between ticks still counts once" $
      let k = keyUp KSpace (keyDown KSpace noKeys)
      in tiSwap (tickInput k) && not (tiSwap (tickInput (spend k)))
  , checkEq "OS auto-repeat of Space cannot swap again" 1
      (gsSwaps (drive (concat (replicate 50 [[keyDown KSpace]]))))
  , checkEq "two separate taps swap twice" 2
      (gsSwaps (drive [[keyDown KSpace], [keyUp KSpace], [], [keyDown KSpace], [keyUp KSpace], []]))
  , check "pausing clears held movement" $
      let k = keyDown KRight noKeys
          k' = afterTick playing (playing { gsPhase = Paused }) k
      in not (tiRight (tickInput k'))
  , check "a key held through a clear stays dead until released, even if it repeats" $
      let k = keyDown KRight (clearHeld (keyDown KRight noKeys))
      in not (tiRight (tickInput k))
           && tiRight (tickInput (keyDown KRight (keyUp KRight k)))
  , check "restart clears held movement" $
      let later = playing { gsTick = 50 }
          k' = afterTick later playing (keyDown KLeft noKeys)
      in not (tiLeft (tickInput k'))
  , check "a normal tick keeps held keys" $
      let k' = afterTick playing playing { gsTick = 1 } (keyDown KLeft noKeys)
      in tiLeft (tickInput k')
  ]
  where
    spend = afterTick playing playing { gsTick = 1 }
    drive frames = fst (foldl' frame (playing, noKeys) frames)
    frame (g, keys) events =
      let keys' = foldl' (\k e -> e k) keys events
          g' = stepGame level (tickInput keys') g
      in (g', afterTick g g' keys')

-- Clock -------------------------------------------------------------------------

clockTests :: [T]
clockTests =
  [ checkEq "one tick's worth of time runs one tick" 1
      (fst (accumulate (1 / 60 + 1e-9) emptyAccumulator))
  , checkEq "a stall is bounded" maxCatchUpTicks (fst (accumulate 5 emptyAccumulator))
  , checkEq "and its excess is dropped, not banked" emptyAccumulator (snd (accumulate 5 emptyAccumulator))
  , checkEq "short frames add up: 600 frames at 60 Hz come to about 600 ticks" True $
      let (total, _) = foldl' (\(n, acc) dt -> let (k, acc') = accumulate dt acc in (n + k, acc'))
                         (0, emptyAccumulator) (replicate 600 (1 / 60))
      in abs (total - 600) <= 1
  , checkEq "negative elapsed time is ignored" 0 (fst (accumulate (-3) emptyAccumulator))
  ]

-- Effects -----------------------------------------------------------------------

fxTests :: [T]
fxTests =
  [ check "effects stay within their bounds through any amount of swapping" $
      let spam = zipWith (\i n -> i { tiSwap = odd n }) (noisyInputs 13 3000) [0 :: Int ..]
          fxs = fxHistory playing spam
      in all (\f -> length (fxParticles f) <= maxParticles
                    && length (fxTrail f) <= maxTrail
                    && length (fxCapsules f) <= maxCapsules) fxs
  , check "an exchange launches a capsule at once, carrying the right amounts" $
      let g1 = stepGame level space playing
          fx = stepFx level playing g1 (initialFx level)
      in case fxCapsules fx of
           (c : _) -> capToA c == 720 && capToB c == 120
           [] -> False
  , check "restart wipes the transitions" $
      let inputs = planInputs level 3000 [AtTick 6]
          states = scanl' (flip (stepGame level)) (initialGame level) (take 120 inputs)
          fx = foldl' (\f (a, b) -> stepFx level a b f) (initialFx level) (zip states (drop 1 states))
          g = last states
          fx' = stepFx level g (stepGame level (noInput { tiRestart = True }) g) fx
      in null (fxCapsules fx') && null (fxParticles fx') && null (fxTrail fx')
           && fxGateA fx' == 1 && fxGateB fx' == 1
  ]
  where
    fxHistory g0 inputs =
      let states = scanl' (flip (stepGame level)) g0 inputs
      in scanl' (\f (a, b) -> stepFx level a b f) (initialFx level) (zip states (drop 1 states))
