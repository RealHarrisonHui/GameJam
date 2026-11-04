-- | Rule tests, with plain assertions. Numbered tests follow the brief's list.
module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.List (foldl')
import Game.Campaign
import Game.Collision
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Solution
import Game.TimeDebt
import Game.Types
import System.Exit (exitFailure)
import Text.Printf (printf)

cfg :: Config
cfg = defaultConfig

level :: Level
level = mkLevel cfg

reach :: Double
reach = cfgPlayerRadius cfg + cfgHazardRadius cfg - cfgContactGrace cfg

-- Time helpers ---------------------------------------------------------------

hold, press, idle :: TimeInput
hold  = TimeInput True False
press = TimeInput True True
idle  = TimeInput False False

times :: Int -> TimeInput -> TimeState -> TimeState
times n input ts = iterate (stepTime cfg input) ts !! n

frozenFor :: Int -> TimeState
frozenFor n = times (n - 1) hold (stepTime cfg press initialTime)

-- Game helpers ---------------------------------------------------------------

playing :: Game
playing = newAttempt cfg level

at :: Vec -> Game
at p = playing { gPlayer = p }

atClock :: Int -> Vec -> Game
atClock c p = (at p) { gTime = initialTime { tsClock = c } }

run :: [TickInput] -> Game -> Game
run inputs g = foldl' (flip (stepGame cfg level)) g inputs

move :: Int -> Int -> TickInput
move x y = noInput { tiMoveX = x, tiMoveY = y }

freezeStart, freezeHold :: TickInput
freezeStart = noInput { tiFreezeHeld = True, tiFreezePressed = True }
freezeHold  = noInput { tiFreezeHeld = True }

replay :: [TickInput] -> [Game]
replay = scanl (flip (stepGame cfg level)) (initialGame cfg level)

-- | A two-wheel-free arena for rules that need custom geometry.
arena :: [Hazard] -> Rect -> Level
arena hazards goal = blankLevel { lvlExit = goal, lvlHazards = hazards }

-- | The gallery floor's two marked refuges.
stillRoomArea, alcoveArea :: Rect
stillRoomArea = safeZones level !! 0
alcoveArea = safeZones level !! 1

-- | Clear of every hazard, through a full cycle, for a player anywhere inside.
zoneIsSafe :: Level -> Rect -> Bool
zoneIsSafe lv (Rect l b r t) = and
  [ vLen (Vec (x - max l (min x r)) (y - max b (min y t))) > reach
  | c <- [0 .. levelCycle lv - 1], Vec x y <- hazardPositions lv c ]

floorGames :: [Level] -> Int -> [Game]
floorGames levels i =
  let lv = levels !! i
  in scanl (flip (stepGame cfg lv)) (newAttempt cfg lv) (floorInputs cfg levels i)

nowhere :: Rect
nowhere = Rect 9000 9000 9001 9001

-- Bots: reactive players used to measure timing windows ---------------------------

towards :: Vec -> Vec -> (Int, Int)
towards (Vec x y) (Vec tx ty) = (axis (tx - x), axis (ty - y))
  where axis d | d > 2 = 1
               | d < -2 = -1
               | otherwise = 0

wheelAt :: Int -> Int -> (Double, Double, Bool)
wheelAt index c =
  let route = hazardRoute (lvlHazards level !! index)
      Vec x y = routePosition route c
      Vec nx _ = routePosition route (c + 1)
  in (x, y, nx > x)

-- | Crossing 1: stop the shuttle wherever it is and walk around it, keeping
-- clear of the stopped wheel.
slotPlan :: Game -> TickInput
slotPlan g =
  let p@(Vec x _) = gPlayer g
      frozen = tsMode (gTime g) == Frozen
      (hx, hy, _) = wheelAt 0 (tsClock (gTime g))
      around = if hy >= 70 then hy - 44 else hy + 44
      target | x < -386  = max 48 (min 92 around)
             | x < -340  = around
             | otherwise = 70
      my = snd (towards p (Vec x target))
      nextAfter (mx, moveY) =
        moveWithWalls (lvlWalls level) (cfgPlayerRadius cfg) p (moveDelta cfg (move mx moveY))
      safe step = vDist (nextAfter step) (Vec hx hy) > reach + 0.5
      (stepX, stepY) = head (filter safe [(1, my), (1, 0), (0, my)] ++ [(0, 0)])
  in noInput { tiMoveX = stepX, tiMoveY = stepY, tiFreezeHeld = True, tiFreezePressed = not frozen }

-- | Crossing 2, the plan from the walkthrough, as a pure policy from the still
-- room to the exit: wait until the wheel is heading east, follow it, freeze once
-- it has turned back but is still beyond the alcove, release in the alcove, wait
-- for it to pass heading west, then run for the exit door.
galleryPlan :: Game -> TickInput
galleryPlan g
  | x >= 470 && y < 214 = go (towards here (Vec 535 70))
  | y > 214 && x >= 422 && frozen = holding (go (0, -1))
  | y > 214 && x >= 422 = go (if x < 533 then (1, if y < 236 then 1 else 0) else (0, -1))
  | y <= 214 && x >= 396 && x <= 452 = inAlcove
  | y <= 214 = inRoom
  | frozen = holding (go (1, 0))
  | mode == Normal && not east && wheel > 465 && wheel <= 530 = holding (go (1, 0))
  | otherwise = go (1, if y < 238 then 1 else 0)
  where
    here@(Vec x y) = gPlayer g
    time = gTime g
    mode = tsMode time
    frozen = mode == Frozen
    (wheel, _, east) = wheelAt 1 (tsClock time)
    go (mx, my) = noInput { tiMoveX = mx, tiMoveY = my }
    holding i = i { tiFreezeHeld = True, tiFreezePressed = not frozen }
    inAlcove
      | frozen && y > 190 = holding (go (0, -1))
      | mode == Normal && not east && wheel < 380 = go (0, 1)
      | otherwise = noInput
    inRoom
      | abs (x + 15) > 3 || y < 186 = go (towards here (Vec (-15) 190))
      | east && wheel > 60 = go (0, 1)
      | otherwise = noInput

runPolicy :: (Game -> TickInput) -> Int -> Game -> Game
runPolicy _ 0 g = g
runPolicy policy n g
  | gPhase g /= Playing = g
  | otherwise = runPolicy policy (n - 1) (stepGame cfg level (policy g) g)

-- Framework ----------------------------------------------------------------------

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  count <- newIORef (0 :: Int)
  let check :: String -> Bool -> IO ()
      check name ok = do
        modifyIORef' count (+ 1)
        printf "  [%s] %s\n" (if ok then "ok" else "FAIL") name
        unless ok (modifyIORef' failures (+ 1))
      section :: String -> IO ()
      section name = printf "%s\n" name

  section "Temporal rules"
  check "01 Normal advances the hazard clock at 1x" $
    tsClock (times 37 idle initialTime) == 37
  check "02 Frozen stops hazard progress" $
    tsClock (frozenFor 50) == 0
  check "03 Frozen accumulates debt" $
    tsDebt (frozenFor 50) == 50 && tsMode (frozenFor 50) == Frozen
  check "04 Releasing a freeze enters repayment" $
    tsMode (stepTime cfg idle (frozenFor 50)) == Repaying
  check "05 Repayment advances the hazard clock at 2x" $
    tsClock (times 10 idle (frozenFor 50)) == 20
  check "06 Repayment reduces debt by one per tick" $
    tsDebt (times 10 idle (frozenFor 50)) == 40
  check "07 Debt never goes negative" $
    all ((>= 0) . tsDebt) (take 500 (iterate (stepTime cfg idle) (frozenFor 30)))
      && tsDebt (stepTime cfg idle (TimeState Repaying 0 5)) == 0
  check "08 The debt cap forces repayment, even with the key held" $
    let capped = frozenFor 180
    in tsMode capped == Repaying && tsDebt capped == 180
       && tsDebt (stepTime cfg hold capped) == 179
  check "09 Freeze cannot restart during repayment" $
    let repaying = stepTime cfg idle (frozenFor 50)
        pressed = stepTime cfg press repaying
    in tsMode pressed == Repaying && tsDebt pressed == tsDebt repaying - 1
  check "10 A fresh press is required after a forced release" $
    let settled = times 180 hold (frozenFor 180)
        heldOn = times 30 hold settled
    in tsMode settled == Normal && tsMode heldOn == Normal && tsDebt heldOn == 0
       && tsMode (stepTime cfg press heldOn) == Frozen
  check "11 A full freeze-and-repay cycle restores cumulative clock progress" $
    all (\n -> let cycleEnd = times n idle (frozenFor n)
               in tsClock cycleEnd == 2 * n && tsMode cycleEnd == Normal && tsDebt cycleEnd == 0)
        [1, 2, 7, 60, 120, 179, 180]
  check "12 Hazards do not teleport at mode transitions" $
    let inputs = replicate 40 noInput ++ [freezeStart] ++ replicate 90 freezeHold
                 ++ replicate 120 noInput ++ [freezeStart] ++ replicate 200 freezeHold
                 ++ replicate 200 noInput
        clocks = map (tsClock . gTime) (scanl (flip (stepGame cfg level)) (at (Vec (-520) 70)) inputs)
        speedOf h = routeLength (hazardRoute h) / fromIntegral (routeOneWay (hazardRoute h))
        fastest = maximum (map speedOf (lvlHazards level))
        steps = zipWith (\a b -> maximum (zipWith vDist (hazardPositions level a) (hazardPositions level b))) clocks (drop 1 clocks)
    in maximum steps <= 2 * fastest + 1e-6 && all (`elem` [0, 1, 2]) (zipWith (-) (drop 1 clocks) clocks)
  check "   Release before any debt accrued returns directly to Normal" $
    let ts = stepTime cfg idle (TimeState Frozen 0 9)
    in tsMode ts == Normal && tsDebt ts == 0
  check "   A press and release inside one tick starts nothing" $
    stepTime cfg (TimeInput False True) initialTime == TimeState Normal 0 1

  section "Gameplay rules"
  let open = Vec (-500) 30  -- middle of the start room
      travelled input g = vDist (gPlayer (run (replicate 20 input) g)) (gPlayer g)
      inMode m = (at open) { gTime = TimeState m 100 0 }
      east = move 1 0
  check "13 Player speed is independent of temporal mode" $
    let held = east { tiFreezeHeld = True }
        distances = [ travelled east (at open), travelled held (inMode Frozen), travelled east (inMode Repaying) ]
    in all (\d -> abs (d - 20 * cfgPlayerSpeed cfg) < 1e-6) distances
  check "14 Diagonal movement is normalized" $
    abs (travelled (move 1 1) (at open) - 20 * cfgPlayerSpeed cfg) < 1e-6
  check "15 Walls block movement" $
    let Vec x _ = gPlayer (run (replicate 200 (move (-1) 0)) (at open))
    in abs (x - (-600 + cfgPlayerRadius cfg)) <= cfgPlayerSpeed cfg
  check "   No wandering path ever overlaps a wall" $
    let lcg :: Int -> Int
        lcg n = (n * 1103515245 + 12345) `mod` 2147483648
        seeds = take 6000 (iterate lcg 7)
        walk p seed =
          let dx = (seed `div` 7) `mod` 3 - 1
              dy = (seed `div` 11) `mod` 3 - 1
          in moveWithWalls (lvlWalls level) (cfgPlayerRadius cfg) p
               (moveDelta cfg (move dx dy))
        path = scanl walk (lvlStart level) seeds
    in not (or [circleHitsRect (cfgPlayerRadius cfg) p w | p <- path, w <- lvlWalls level])
  check "16 Frozen hazards remain lethal" $
    -- Stop the shuttle, then walk into it while its clock stands still.
    let begin = atClock 7 (Vec (-392) 70)
        Vec _ hy = head (hazardPositions level 7)
        walk = freezeStart { tiMoveX = 1 } : replicate 60 (freezeHold { tiMoveX = 1, tiMoveY = signum (round (hy - 70)) })
        final = run walk begin
    in gPhase final == Lost && tsClock (gTime final) == 7 && tsMode (gTime final) /= Normal
  check "17 Accelerated hazards cannot tunnel through the player" $
    -- 300 px per hazard tick: the wheel is 150 px either side of the player at
    -- consecutive samples, so only the sweep can see the contact.
    let fast = arena [Hazard (Route [Vec (-30000) 0, Vec 30000 0] 200 0)] nowhere
        c0 = 99
        p = Vec 150 0
        samples = [routePosition (hazardRoute (head (lvlHazards fast))) c | c <- [c0, c0 + 1, c0 + 2]]
        repaying = (newAttempt cfg fast) { gPlayer = p, gTime = TimeState Repaying 50 c0 }
        hit = stepGame cfg fast noInput repaying
    in all (\h -> vDist h p > reach) samples && gPhase hit == Lost
  check "   Sweep agrees with dense sampling at the real 2x speed" $
    let samplesOf c = [routePosition (hazardRoute h) c | h <- lvlHazards level]
        dense p c0 = or [ vDist p (vLerp (fromIntegral k / 64) a b) <= reach
                        | (a, b) <- zip (samplesOf c0) (samplesOf (c0 + 1))
                                 ++ zip (samplesOf (c0 + 1)) (samplesOf (c0 + 2))
                        , k <- [0 .. 64 :: Int] ]
        cases = [ (Vec x y, c) | x <- [-373, -360 .. -300], y <- [-5, 15 .. 145], c <- [0 .. 29] ]
        agrees (p, c) = (hazardHit cfg level c (c + 2) p p /= Nothing) == dense p c
    in all agrees cases
  check "18 Pause stops debt and hazard progression" $
    let frozen = run [freezeStart, freezeHold, freezeHold] playing
        paused = stepGame cfg level noInput { tiPausePressed = True } frozen
        later = run (replicate 300 freezeHold { tiMoveX = 1 }) paused
    in gPhase paused == Paused && later == paused
  check "   Resuming from a freeze enters repayment, not the stale held key" $
    let frozen = run [freezeStart, freezeHold, freezeHold] playing
        paused = stepGame cfg level noInput { tiPausePressed = True } frozen
        resumed = stepGame cfg level freezeHold { tiPausePressed = True } paused
        after = stepGame cfg level freezeHold resumed
    in tsMode (gTime resumed) == Repaying && tsDebt (gTime resumed) == 3
       && tsMode (gTime after) == Repaying && tsDebt (gTime after) == 2
  check "   Resuming from a zero-debt Frozen state enters Normal" $
    tsMode (resumeFromPause (TimeState Frozen 0 3)) == Normal
  check "   Pausing during repayment resumes repayment" $
    resumeFromPause (TimeState Repaying 9 3) == TimeState Repaying 9 3
  check "19 Restart restores the initial state" $
    let messy = run (freezeStart : replicate 40 freezeHold { tiMoveX = 1 }) playing
        restarted = stepGame cfg level noInput { tiRestartPressed = True } messy
    in restarted == playing && gPlayer restarted == lvlStart level
       && tsMode (gTime restarted) == Normal && tsDebt (gTime restarted) == 0 && tsClock (gTime restarted) == 0
  check "   Restart resets transient effects" $
    let messy = run (freezeStart : replicate 40 freezeHold { tiMoveX = 1 }) playing
        restart = noInput { tiRestartPressed = True }
        fx = stepFx cfg level messy (stepGame cfg level restart messy)
               initialFx { fxTick = 5, fxFrost = 1, fxMeter = 0.5, fxRings = [Ring (Vec 0 0) 0 9 RingFreeze] }
    in fx == initialFx { fxTick = 6 }
  check "20 A hazard contact beats simultaneous exit contact" $
    let sameSpot = Rect (-5) (-5) 5 5
        lv = arena [Hazard (Route [Vec 0 0, Vec 1 0] 10 0)] sameSpot
        final = stepGame cfg lv noInput (newAttempt cfg lv) { gPlayer = Vec 0 0 }
        safeLv = arena [] sameSpot
        winning = stepGame cfg safeLv noInput (newAttempt cfg safeLv) { gPlayer = Vec 0 0 }
    in gPhase final == Lost && gHitBy final == Just 0 && gPhase winning == Won
  check "21 Same start and tick inputs give the same result" $
    let a = replay solutionInputs
        b = replay solutionInputs
    in a == b && last a == last b

  section "Input layer"
  let pressKey = applyKey Press
      releaseKey = applyKey Release
      tickOf = advanceFrame cfg (\ti n -> (False, ti : n))
      stepper input = Stepper 0 input
      dtTicks k = fromIntegral (k :: Int) / 60 :: Double
  check "   A one-shot press reaches exactly one tick across catch-up" $
    let input = pressKey GkSpace emptyInput
        (_, ticks) = tickOf (dtTicks 4 + 0.001) (stepper input) []
    in length ticks == 4 && length (filter tiFreezePressed ticks) == 1 && all tiFreezeHeld ticks
  check "   Key repeat does not create a second press" $
    let once = consumeOneShots (pressKey GkSpace emptyInput)
    in not (tiFreezePressed (tickInput (pressKey GkSpace once)))
  check "   Long frames are clamped and catch-up is bounded" $
    let (st, ticks) = tickOf 5 (stepper emptyInput) []
    in length ticks == cfgMaxCatchUp cfg && stAccumulator st == 0
  check "   Fractional time carries over to the next frame" $
    let (st, ticks) = tickOf (dtTicks 1 / 2) (stepper emptyInput) []
        (_, ticks2) = tickOf (dtTicks 1 * 0.6) st ticks
    in null ticks && length ticks2 == 1
  check "   Arrow keys and WASD drive the same axes and hold independently" $
    let both = pressKey GkD (pressKey GkRight emptyInput)
        one = releaseKey GkD both
        none = releaseKey GkRight one
    in tiMoveX (tickInput both) == 1 && tiMoveX (tickInput one) == 1 && tiMoveX (tickInput none) == 0
  check "   Pause, restart and screen changes clear held input" $
    let inGame = run [noInput] playing
        pauseIn = noInput { tiPausePressed = True }
        restartIn = noInput { tiRestartPressed = True }
        enterIn = noInput { tiConfirmPressed = True }
        title = initialGame cfg level
    in inputMustClear pauseIn inGame (stepGame cfg level pauseIn inGame)
       && inputMustClear restartIn inGame (stepGame cfg level restartIn inGame)
       && inputMustClear enterIn title (stepGame cfg level enterIn title)
       && not (inputMustClear noInput inGame (stepGame cfg level noInput inGame))
  check "   P toggles once per press through the frame loop" $
    let world ti g = let g' = stepGame cfg level ti g in (inputMustClear ti g g', g')
        input = pressKey GkP emptyInput
        (st, g1) = advanceFrame cfg world (dtTicks 3 + 0.001) (stepper input) playing
        (_, g2) = advanceFrame cfg world (dtTicks 3) st g1
    in gPhase g1 == Paused && gPhase g2 == Paused
  check "   Screen flow: title -> playing -> lost -> retry" $
    let title = initialGame cfg level
        started = stepGame cfg level noInput { tiConfirmPressed = True } title
        lost = started { gPhase = Lost, gPlayer = Vec 1 1 }
        retried = stepGame cfg level noInput { tiConfirmPressed = True } lost
    in gPhase title == Title && stepGame cfg level noInput title == title
       && started == playing && retried == playing

  section "Level validation"
  let bounds = Rect (-640) (-290) 640 360
      Rect bl bb br bt = bounds
      grid = [ Vec x y | x <- takeWhile (< br) [bl + 0.5, bl + 4.5 ..], y <- takeWhile (< bt) [bb + 0.5, bb + 4.5 ..] ]
      inWall p = any (`rectContains` p) (lvlWalls level)
      inFloor p = any (`rectContains` p) (lvlFloors level)
      cycleTicks = [0 .. levelCycle level - 1]
      clearOfHazards area = and
        [ distanceToRect h area > reach | c <- cycleTicks, h <- hazardPositions level c ]
      distanceToRect (Vec x y) (Rect l b r t) =
        vLen (Vec (x - max l (min x r)) (y - max b (min y t)))
      shrink m (Rect l b r t) = Rect (l + m) (b + m) (r - m) (t - m)
      playerSpot = shrink (cfgPlayerRadius cfg)
  let badCells = filter (\p -> inWall p == inFloor p) grid
  unless (null badCells) (print (take 8 badCells, length badCells))
  check "   Walls and floors partition the board" $
    null badCells
  check "   The level has exactly two hazards" $
    length (lvlHazards level) == 2
  check "   The start point is inside the marked start area" $
    rectContains (lvlStartArea level) (lvlStart level)
  check "   Start area is safe through the full hazard cycle, at any speed" $
    clearOfHazards (lvlStartArea level)
  check "   Recovery room is safe through the full hazard cycle, at any speed" $
    clearOfHazards stillRoomArea
  check "   Refuge alcove is safe through the full hazard cycle" $
    clearOfHazards alcoveArea
  check "   Exit area is safe through the full hazard cycle" $
    clearOfHazards (lvlExit level)
  check "   Hazard routes stay inside the floors" $
    and [ inFloor h && not (inWall h) | c <- cycleTicks, h <- hazardPositions level c ]
  let hazardsReach p = any (\c -> any (\h -> vDist p h <= reach) (hazardPositions level c)) cycleTicks
      freeSpots rect = [ Vec x y | let Rect l b r t = playerSpot rect
                       , x <- [l, l + 3 .. r], y <- [b, b + 3 .. t]
                       , not (any (circleHitsRect (cfgPlayerRadius cfg) (Vec x y)) (lvlWalls level)) ]
      slotRect = Rect (-386) (-10) (-294) 150
      galleryRect = Rect (-70) 227 600 306
      spots = freeSpots slotRect ++ freeSpots galleryRect
  check (printf "   No hiding pocket: all %d slot and gallery spots get swept" (length spots)) $
    all hazardsReach spots

  section "Timing windows"
  let throughSlot g = let Vec x _ = gPlayer g in x > -280
      slotPolicyRun n g
        | n == (0 :: Int) || gPhase g /= Playing || throughSlot g = g
        | otherwise = slotPolicyRun (n - 1) (stepGame cfg level (slotPlan g) g)
      slotAttempts = [ slotPolicyRun 120 (atClock c (Vec (-392) 70)) | c <- [0 .. 29] ]
      slotDone g = gPhase g == Playing && throughSlot g
      slotWins = length (filter slotDone slotAttempts)
      slotDebt = maximum [ tsDebt (gTime g) | g <- slotAttempts ]
      straightOk = length [ () | c <- [0 .. 29 :: Int]
                          , gPhase (run (freezeStart { tiMoveX = 1 } : replicate 35 freezeHold { tiMoveX = 1 })
                                        (atClock c (Vec (-392) 70))) /= Lost ]
  printf "      freezing then walking around the wheel crosses from %d/30 shuttle phases (worst case %d ticks of debt)\n" slotWins slotDebt
  printf "      freezing then walking dead straight crosses from %d/30 phases\n" straightOk
  check "   Crossing 1: a freeze at any shuttle phase can be walked around" $
    slotWins == 30
  let entries = [0 .. 179]
      doorStart c = atClock c (Vec (-15) 180)
      results = [ runPolicy galleryPlan 1400 (doorStart c) | c <- entries ]
  let
      wins = length (filter ((== Won) . gPhase) results)
      slowest = maximum (0 : [ gTicks g | g <- results, gPhase g == Won ])
  printf "      the walkthrough policy wins from %d/180 clock phases at the door (slowest %d ticks)\n" wins slowest
  check "   Crossing 2: the walkthrough plan works from every clock phase at the door" $
    wins == 180
  let freezeAt f =
        -- Enter behind the wheel as in the scripted run, then freeze after f ticks of walking.
        let walk = replicate 15 (move 0 1) ++ replicate f (move 1 0)
            entered = run walk (doorStart 173)
            Vec x _ = gPlayer entered
            toMouth = max 0 (ceiling ((424 - x) / cfgPlayerSpeed cfg)) :: Int
            frozenRun = freezeStart { tiMoveX = 1 } : replicate (toMouth - 1) freezeHold { tiMoveX = 1 }
                        ++ replicate 20 freezeHold { tiMoveY = -1 }
            final = run frozenRun entered
            Vec _ y = gPlayer final
        in gPhase entered /= Lost && gPhase final == Playing && y < 200
           && tsMode (gTime final) == Frozen
      goodFreezes = filter freezeAt [0 .. 120]
  printf "      following the wheel, the freeze can start after %s ticks of walking (%d of 121 work)\n"
    (show (head goodFreezes, last goodFreezes)) (length goodFreezes)
  check "   Crossing 2: the freeze has a usable window of over half a second" $
    length goodFreezes >= 30
  let releaseEarly n =
        -- Same run, but let go n ticks before reaching the alcove mouth.
        let walk = replicate 15 (move 0 1) ++ replicate 52 (move 1 0)
            entered = run walk (doorStart 173)
            Vec x _ = gPlayer entered
            toMouth = ceiling ((424 - x) / cfgPlayerSpeed cfg) :: Int
            inputs = freezeStart { tiMoveX = 1 } : replicate (toMouth - 1 - n) freezeHold { tiMoveX = 1 }
                     ++ replicate n (move 1 0) ++ replicate 20 (move 0 (-1)) ++ replicate 200 noInput
        in gPhase (run inputs entered) == Lost
      fatal = filter releaseEarly [0 .. 60]
  printf "      in the scripted timing, releasing this many ticks short of the alcove is fatal: %s\n" (show fatal)
  check "   Crossing 2: releasing too early is punished, releasing in time is not" $
    not (releaseEarly 0) && releaseEarly 30
  let games = replay solutionInputs
      final = last games
      firstWon = head (filter ((== Won) . gPhase) games)
      modes = map (tsMode . gTime) games
      safeRepay = [ g | g <- games, tsMode (gTime g) == Repaying
                      , rectContains stillRoomArea (gPlayer g) ]
  printf "      scripted run: won after %d ticks (%.1f s), %d ticks borrowed\n"
    (gTicks firstWon) (fromIntegral (gTicks firstWon) / 60 :: Double) (gBorrowed firstWon)
  check "   The scripted solution wins through the real logic" $
    gPhase final == Won
  check "   The scripted solution freezes, repays in the still room, and freezes again" $
    Frozen `elem` modes && Repaying `elem` modes && not (null safeRepay)
      && length (filter id (zipWith (\a b -> a /= Frozen && b == Frozen) modes (drop 1 modes))) == 2

  section "Presentation bounds"
  let fxRun config = scanl (\fx (a, b) -> stepFx config level a b fx) initialFx (zip games (drop 1 games))
      deathInputs = replicate 1 noInput { tiConfirmPressed = True } ++ replicate 200 (move 1 0)
      deathGames = replay deathInputs
      deathFx = scanl (\fx (a, b) -> stepFx cfg level a b fx) initialFx (zip deathGames (drop 1 deathGames))
      withinBounds config fx =
        length (fxParticles fx) <= cfgMaxParticles config
          && length (fxRings fx) <= cfgMaxRings config
          && all ((<= cfgTrailLength config) . length) (fxTrails fx)
  check "   Particles, rings and trails stay bounded over a whole run" $
    all (withinBounds cfg) (fxRun cfg) && all (withinBounds cfg) deathFx
  check "   A walk straight into the slot dies and produces impact effects" $
    gPhase (last deathGames) == Lost && any (not . null . fxRings) deathFx
  check "   Reduced effects drops sparks and trails" $
    let reduced = cfg { cfgReducedEffects = True }
    in all (\fx -> null (fxParticles fx) && null (fxTrails fx)) (fxRun reduced)

  section "Early brake presses"
  let repayingAt debt = playing { gPlayer = Vec (-520) 70, gTime = TimeState Repaying debt 0 }
      still = noInput
      pressBrake = noInput { tiFreezeHeld = True, tiFreezePressed = True }
      holdBrake = noInput { tiFreezeHeld = True }
      after inputs g = foldl' (flip (stepGame cfg level)) g inputs
      buffer = cfgFreezeBuffer cfg
      pressedAt debt = after (pressBrake : replicate (debt + 5) holdBrake) (repayingAt debt)
  check "   A press in the last moments of repayment takes effect when the debt clears" $
    tsMode (gTime (pressedAt buffer)) == Frozen && tsDebt (gTime (pressedAt buffer)) > 0
  check "   A press with more debt left than the buffer is ignored, as before" $
    tsMode (gTime (pressedAt (buffer + 1))) == Normal
  check "   Letting go cancels a buffered press" $
    let g = after (pressBrake : still : replicate 30 still) (repayingAt 3)
    in tsMode (gTime g) == Normal && not (gQueued g)
  check "   A key held through a forced release still does not re-freeze" $
    let capped = after (pressBrake : replicate 500 holdBrake)
          (playing { gPlayer = Vec (-520) 70 })
    in tsMode (gTime capped) == Normal && tsDebt (gTime capped) == 0

  section "Landing ghosts"
  let repayLine = iterate (stepTime cfg idle) (frozenFor 60)
      settled = map (settledPositions level) (take 60 repayLine)
  check "   A ghost stays put for the whole repayment" $
    all (== head settled) settled
  check "   Every wheel arrives exactly at its ghost when the debt clears" $
    hazardPositions level (tsClock (repayLine !! 60)) == head settled
  check "   Holding the key moves the ghost forward" $
    settledPositions level (frozenFor 10) /= settledPositions level (frozenFor 30)

  section "The campaign"
  let levels = mkLevels cfg
      floorIndices = [0 .. length levels - 1]
      won i = gPhase (last (floorGames levels i)) == Won
      modesOf i = map (tsMode . gTime) (floorGames levels i)
      freezesOn i = let m = modesOf i
                    in length (filter id (zipWith (\a b -> a /= Frozen && b == Frozen) m (drop 1 m)))
      maxDebtOn i = maximum (map (tsDebt . gTime) (floorGames levels i))
      repayOutside i =
        length [ () | g <- floorGames levels i, tsMode (gTime g) == Repaying
                    , not (any (`rectContains` gPlayer g) (safeZones (levels !! i))) ]
  mapM_ (\i -> printf "      floor %d %-18s %4d ticks, %d freezes, peak debt %3d, %3d repaying ticks outside marked zones\n"
                 (i + 1) (lvlTitle (levels !! i)) (gTicks (last (floorGames levels i)))
                 (freezesOn i) (maxDebtOn i) (repayOutside i)) floorIndices
  check "   Every floor has a run that clears it through the real rules" $
    all won floorIndices
  check "   Every floor's marked zones are safe through the full hazard cycle" $
    and [ zoneIsSafe lv z | lv <- levels, z <- safeZones lv ]
  check "   Every start point is safe and inside its start area" $
    and [ rectContains (lvlStartArea lv) (lvlStart lv) && zoneIsSafe lv (lvlStartArea lv) | lv <- levels ]
  check "   No wall covers a start point or an exit" $
    and [ not (any (circleHitsRect (cfgPlayerRadius cfg) (lvlStart lv)) (lvlWalls lv))
          && not (any (\(Rect l b r t) -> let Rect a c d e = lvlExit lv in l < d && a < r && b < e && c < t) (lvlWalls lv))
        | lv <- levels ]
  check "   Every floor needs the brake at least once" $
    all ((>= 1) . freezesOn) floorIndices
  check "   The Counting Room needs a separate brake for each shaft" $
    freezesOn 2 == 3
  check "   The Mainspring repays debt away from every marked zone" $
    repayOutside 4 > 0
  check "   Every floor has a name and a note" $
    all (\lv -> not (null (lvlTitle lv)) && not (null (lvlBrief lv)) && not (null (lvlHint lv))) levels
  let campaignRun = scanl (flip (stepCampaign cfg levels)) (newCampaign cfg levels) (campaignInputs cfg levels)
      lastStep = last campaignRun
  printf "      the whole campaign takes %d ticks (%.1f s) when played perfectly\n"
    (cTotalTicks lastStep) (fromIntegral (cTotalTicks lastStep) / 60 :: Double)
  check "   The scripted campaign reaches the ending with no deaths" $
    cScreen lastStep == OnEnding && cTotalDeaths lastStep == 0
  check "   The scripted campaign visits every floor in order" $
    let visited = foldr (\c acc -> if take 1 acc == [cIndex c] then acc else cIndex c : acc) [] campaignRun
    in visited == floorIndices
  let onFloor0 = (stepCampaign cfg levels confirmKey (stepCampaign cfg levels confirmKey (newCampaign cfg levels)))
      confirmKey = noInput { tiConfirmPressed = True }
      walkIntoWheel = replicate 200 (move 1 0)
      afterDeath = foldl' (flip (stepCampaign cfg levels)) onFloor0 walkIntoWheel
      retried = stepCampaign cfg levels confirmKey afterDeath
      afterTwo = foldl' (flip (stepCampaign cfg levels)) retried walkIntoWheel
  check "   A death is counted and retries the same floor" $
    cDeaths afterDeath == 1 && gPhase (cGame retried) == Playing && cIndex retried == 0
  check "   Hints appear only after failing twice" $
    not (hintsDue onFloor0) && not (hintsDue afterDeath) && hintsDue afterTwo
  check "   Clearing a floor forgets its deaths" $
    let cleared = foldl' (flip (stepCampaign cfg levels)) onFloor0 (floorInputs cfg levels 0)
        dead = cleared { cDeaths = 5 }
    in cDeaths (stepCampaign cfg levels confirmKey dead) == 0
  check "   Held keys are forgotten when the floor changes" $
    let cleared = foldl' (flip (stepCampaign cfg levels)) onFloor0 (floorInputs cfg levels 0)
        next = stepCampaign cfg levels confirmKey cleared
    in campaignMustClear confirmKey cleared next
  check "   Teaching prompts follow the player's state" $
    let lv = head levels
        fresh = newAttempt cfg lv
    in activePrompt lv fresh == Just "WASD OR ARROWS TO MOVE"
       && activePrompt lv fresh { gPlayer = Vec (-420) 70 } == Just "HOLD SPACE"
       && activePrompt lv fresh { gTime = TimeState Frozen 5 0, gPlayer = Vec (-340) 40 } == Just "KEEP HOLDING. WALK AROUND IT"

  failed <- readIORef failures
  total <- readIORef count
  printf "%d checks, %d failed\n" total failed
  unless (failed == 0) exitFailure
