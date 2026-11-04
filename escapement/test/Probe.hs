-- | Breadth-first search over the real 'stepGame', with freezing disabled.
--
-- Answers "can each crossing be done just by timing and dodging?". States are
-- (clock mod cycle, position rounded to a grid cell); the first arrival in a
-- cell stands for all of them, so a "no route" answer is strong evidence, not a
-- proof. A found route is a real one, since it replays through the real logic.
module Main (main) where

import Control.Monad (forM)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.Maybe (catMaybes)
import Game.Collision (moveWithWalls, rectContains)
import Game.Level
import Game.Logic
import Game.Types
import Text.Printf (printf)

data Verdict = Verdict
  { vTicks  :: Maybe Int  -- ^ ticks to the goal, if reached
  , vStates :: !Int
  , vBest   :: !Double    -- ^ closest approach to the goal, in pixels
  , vTrail  :: [(Int, Vec, [Vec])]  -- ^ a witness route: tick, player, hazards
  }

moves :: [TickInput]
moves = [noInput { tiMoveX = x, tiMoveY = y } | x <- [-1, 0, 1], y <- [-1, 0, 1]]

search :: Config -> Level -> Double -> Vec -> (Game -> Bool) -> Vec -> IO Verdict
search cfg level cell start goal target = do
  let cycleLen = levelCycle level
      cols = ceiling (1280 / cell) + 1 :: Int
      rows = ceiling (720 / cell) + 1 :: Int
      index clock (Vec x y) =
        (clock `mod` cycleLen) * cols * rows
          + floor ((x + 640) / cell) * rows + floor ((y + 360) / cell)
  seen <- newArray (0, cycleLen * cols * rows) False :: IO (IOUArray Int Bool)
  let begin = (newAttempt cfg level) { gPlayer = start }
      loop :: Int -> Int -> Double -> [(Game, [(Int, Vec, [Vec])])] -> IO Verdict
      loop _ states best [] = pure (Verdict Nothing states best [])
      loop tick states best frontier
        | ((_, trail) : _) <- filter (goal . fst) frontier = pure (Verdict (Just tick) states 0 (reverse trail))
        | otherwise = do
            next <- forM frontier $ \(g, trail) -> forM moves $ \m -> do
              let g' = stepGame cfg level m g
              if gPhase g' == Lost then pure Nothing else do
                let i = index (tsClock (gTime g')) (gPlayer g')
                dup <- readArray seen i
                if dup then pure Nothing else do
                  writeArray seen i True
                  let c = tsClock (gTime g')
                      trail' = if c `mod` 30 == 0 then (c, gPlayer g', hazardPositions level c) : trail else trail
                  pure (Just (g', trail'))
            let frontier' = catMaybes (concat next)
                best' = minimum (best : map (vDist target . gPlayer . fst) frontier')
            loop (tick + 1) (states + length frontier') best' frontier'
  loop 0 1 (vDist start target) [(begin, [])]

-- | Hold right from just outside the slot, no freezing, from each clock phase.
straightWalks :: Config -> Level -> Double -> Int
straightWalks cfg level y = length (filter survives [0 .. levelCycle level - 1])
  where
    right = noInput { tiMoveX = 1 }
    survives clock =
      let begin = (newAttempt cfg level)
            { gPlayer = Vec (-392) y, gTime = initialTime { tsClock = clock } }
      in gPhase (iterate (stepGame cfg level right) begin !! 40) /= Lost

report :: String -> Verdict -> IO ()
report name v = do
  printf "%-44s %s  (%d states, closest %.0f px)\n" name
    (maybe "NO ROUTE FOUND" (\t -> "route in " ++ show t ++ " ticks") (vTicks v))
    (vStates v) (vBest v)
  mapM_ (\(t, Vec x y, hs) -> printf "  t=%4d player (%6.0f,%6.0f)  hazards %s\n" t x y
          (unwords [printf "(%.0f,%.0f)" a b | Vec a b <- hs])) (vTrail v)

-- | Could a floor be cleared without ever touching the brake?
noBrake :: Config -> Int -> Level -> IO ()
noBrake cfg i level = do
  let Vec ex ey = (\(Rect l b r t) -> Vec ((l + r) / 2) ((b + t) / 2)) (lvlExit level)
  search cfg level 3 (lvlStart level) ((== Won) . gPhase) (Vec ex ey)
    >>= \v -> report (printf "Floor %d %s, no brake" (i + 1 :: Int) (lvlTitle level)) v { vTrail = [] }

-- | Floor 3 promises that no single hold crosses two shafts. Stop the clock at
-- every phase and flood-fill what a held brake can reach, ignoring the cap.
oneHoldReach :: Config -> Level -> IO ()
oneHoldReach cfg level = do
  let wheels = lvlHazards level
      reach = cfgPlayerRadius cfg + cfgHazardRadius cfg - cfgContactGrace cfg
      cell = 3 :: Double
      cols = ceiling (1280 / cell) + 1 :: Int
      rows = ceiling (720 / cell) + 1 :: Int
      key (Vec x y) = floor ((x + 640) / cell) * rows + floor ((y + 360) / cell)
      step p (mx, my) = moveWithWalls (lvlWalls level) (cfgPlayerRadius cfg) p
        (moveDelta cfg noInput { tiMoveX = mx, tiMoveY = my })
      phase c = do
        seen <- newArray (0, cols * rows) False :: IO (IOUArray Int Bool)
        let spots = [routePosition (hazardRoute w) c | w <- wheels]
            clear p = all (\h -> vDist p h > reach) spots
            flood :: [Vec] -> Vec -> IO Vec
            flood [] far = pure far
            flood frontier far = do
              next <- forM frontier $ \p -> forM [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]] $ \m -> do
                let q = step p m
                dup <- readArray seen (key q)
                if dup || not (clear q) then pure Nothing else do
                  writeArray seen (key q) True
                  pure (Just q)
              let frontier' = catMaybes (concat next)
                  Vec far' _ = foldl (\a@(Vec ax _) b@(Vec bx _) -> if bx > ax then b else a) far frontier'
              flood frontier' (Vec far' 0)
            start = Vec (-362) 35
        if clear start then flood [start] start else pure start
  fars <- mapM phase [0 .. levelCycle level - 1]
  let furthest = maximum [x | Vec x _ <- fars]
  printf "Floor 3, one hold from the first shaft's door: furthest east reached at any clock phase is x=%.0f (second shaft's far door is at x=38)\n" furthest

main :: IO ()
main = do
  let cfg = defaultConfig
      level = mkLevel cfg
      levels = mkLevels cfg
      inRoom = rectContains (safeZones level !! 0) . gPlayer
      won g = gPhase g == Won
      roomCentre = Vec (-120) 60
      exitCentre = Vec 535 70
      doorEast = Vec (-270) 70
  printf "cycle = %d ticks\n" (levelCycle level)
  mapM_ (\y -> printf "Straight walk through the slot at y=%.0f, every start tick: %d of %d survive\n" y
          (straightWalks cfg level y) (levelCycle level)) [48, 60, 70, 80, 92]
  mapM_ (\cell -> do
    printf "-- grid %.1f px\n" cell
    search cfg level cell (lvlStart level) inRoom doorEast
      >>= report "Crossing 1, no freeze: start -> still room"
    search cfg level cell roomCentre won exitCentre
      >>= report "Crossing 2, no freeze: still room -> exit") [3, 2]
  printf "-- every floor, grid 3 px\n"
  mapM_ (\(i, lv) -> noBrake cfg i lv) (zip [0 ..] levels)
  oneHoldReach cfg (levels !! 2)
