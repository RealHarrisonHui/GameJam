-- | A complete scripted run of every floor, as literal tick input. The tests
-- replay these through the real rules, and @--demo@ plays them on screen.
module Game.Solution
  ( Segment (..)
  , solutionScript
  , expandScript
  , solutionInputs
  , floorInputs
  , campaignInputs
  ) where

import Game.Collision (moveWithWalls)
import Game.Level
import Game.Logic
import Game.Types

data Segment = Segment
  { segTicks :: !Int
  , segX     :: !Int
  , segY     :: !Int
  , segHold  :: !Bool  -- ^ Space held throughout this segment
  } deriving (Eq, Show)

-- | Floor 4: the slot, then the gallery.
solutionScript :: [Segment]
solutionScript =
  [ Segment 41 1 0 False      -- walk to the slot door
  , Segment 4 0 0 False       -- wait for the shuttle to reach the bottom of its stroke
  , Segment 36 1 0 True       -- freeze it and cross the slot
  , Segment 48 1 1 False      -- release in the still room; drift north-east while repaying
  , Segment 45 1 0 False      -- under the north door
  , Segment 15 0 1 False      -- through the door, just behind the east-bound wheel
  , Segment 52 1 0 False      -- follow the wheel along the gallery
  , Segment 83 1 0 True       -- freeze it as it turns back; run for the alcove
  , Segment 17 0 (-1) True    -- step down into the alcove, still frozen
  , Segment 100 0 0 False     -- release; wait out the debt while the wheel screams past
  , Segment 20 0 1 False      -- back up into the gallery behind it
  , Segment 34 1 0 False      -- along to the exit door
  , Segment 40 0 (-1) False   -- down onto the exit
  ]

-- | Floor 1: the same first crossing, then straight on to the exit.
undercroftScript :: [Segment]
undercroftScript =
  [ Segment 41 1 0 False
  , Segment 4 0 0 False
  , Segment 36 1 0 True
  , Segment 300 1 0 False
  ]

-- | One tick of Enter to leave the title, then the segments. A hold that begins
-- after no hold gets a press edge on its first tick only.
expandScript :: [Segment] -> [TickInput]
expandScript segments = noInput { tiConfirmPressed = True } : go False segments
  where
    go _ [] = []
    go wasHeld (Segment n x y hold : rest) =
      [ noInput { tiMoveX = x, tiMoveY = y, tiFreezeHeld = hold
                , tiFreezePressed = hold && not wasHeld && i == 0 }
      | i <- [0 .. n - 1 :: Int] ] ++ go hold rest

solutionInputs :: [TickInput]
solutionInputs = expandScript solutionScript

-- Bots: small reactive players, run once to produce a literal input list ------

-- | Run a bot through the real rules until the floor is cleared, keeping its
-- inputs. A bot's own press edges are derived here, as a keyboard would make them.
botInputs :: Config -> Level -> (Game -> TickInput) -> [TickInput]
botInputs cfg level bot = go (6000 :: Int) False (newAttempt cfg level)
  where
    go 0 _ _ = []
    go n wasHeld g
      | gPhase g /= Playing = []
      | otherwise =
          let wanted = bot g
              input = wanted { tiFreezePressed = tiFreezeHeld wanted && not wasHeld }
          in input : go (n - 1) (tiFreezeHeld wanted) (stepGame cfg level input g)

-- | The clock position of a hazard, on its own axis.
wheelY, wheelX :: Level -> Int -> Int -> Double
wheelY level i c = let Vec _ y = routePosition (hazardRoute (lvlHazards level !! i)) c in y
wheelX level i c = let Vec x _ = routePosition (hazardRoute (lvlHazards level !! i)) c in x

heading :: Level -> Int -> Int -> Bool
heading level i c = wheelX level i (c + 1) > wheelX level i c

-- | Cross a run of shafts: wait outside each one until its wheel is near the end
-- of its track, brake, walk around it, then let go on the far side.
shaftBot :: Config -> Level -> Double -> [(Double, Int)] -> Game -> TickInput
shaftBot cfg level clearance shaftList g = case filter (\(cx, _) -> x < cx + 50) shaftList of
  [] -> noInput { tiMoveX = 1, tiMoveY = lane }
  ((cx, i) : _)
    | x < cx - 50 -> noInput { tiMoveX = 1, tiMoveY = lane }
    | frozen -> crossing cx i
    | x >= cx - 46 -> crossing cx i
    | mode == Normal && abs (wheelY level i clock - cy) >= clearance -> crossing cx i
    | otherwise -> noInput
  where
    Vec x y = gPlayer g
    time = gTime g
    mode = tsMode time
    clock = tsClock time
    frozen = mode == Frozen
    cy = lvlStartY
    lvlStartY = let Vec _ sy = lvlStart level in sy
    lane | y < cy - 2 = 1
         | y > cy + 2 = -1
         | otherwise = 0
    crossing cx i =
      let wy = wheelY level i clock
          around = if wy >= cy then wy - 44 else wy + 44
          -- Stay level with the doors near the shaft walls, so a corner never catches.
          target | x > cx - 26 && x < cx + 20 = around
                 | otherwise = max (cy - 7) (min (cy + 7) around)
          my | y < target - 2 = 1
             | y > target + 2 = -1
             | otherwise = 0 :: Int
          reach = cfgPlayerRadius cfg + cfgHazardRadius cfg - cfgContactGrace cfg
          next (mx, moveY) = moveWithWalls (lvlWalls level) (cfgPlayerRadius cfg) (gPlayer g)
            (moveDelta cfg noInput { tiMoveX = mx, tiMoveY = moveY })
          safe step = vDist (next step) (Vec cx wy) > reach + 0.5
          (stepX, stepY) = head (filter safe [(1, my), (1, 0), (0, my)] ++ [(0, 0)])
      in noInput { tiMoveX = stepX, tiMoveY = stepY, tiFreezeHeld = True }

-- | Floor 5: follow the long wheel in, brake once for the shaft and pay that debt
-- while walking, then brake again as the long wheel comes back, and let it pass
-- from the alcove.
mainspringBot :: Level -> Game -> TickInput
mainspringBot level g
  | y < 150 && x < -460 = walk (0, 1)
  | x >= 420 && y < 212 = walk (towards (Vec 500 (-50)))
  | inAlcove = alcove
  | x > 360 = walk (if x < 475 then 1 else 0, if x < 475 then lane else -1)
  | x < -452 = walk (1, lane)
  | x < -346 = crossShaft
  | frozen && long > 395 = hold (walk intoAlcove)
  -- Brake as late as the window allows: from any earlier, three seconds will not
  -- reach the alcove.
  | mode == Normal && long > 395 && long < 430 && not longEast = hold (walk intoAlcove)
  | otherwise = walk (1, lane)
  where
    Vec x y = gPlayer g
    time = gTime g
    mode = tsMode time
    clock = tsClock time
    frozen = mode == Frozen
    long = wheelX level 0 clock
    longEast = heading level 0 clock
    shaftY = wheelY level 1 clock
    walk (mx, my) = noInput { tiMoveX = mx, tiMoveY = my }
    hold i = i { tiFreezeHeld = True }
    lane | y < 164 = 1
         | y > 168 = -1
         | otherwise = 0
    towards (Vec tx ty) = (axis (tx - x), axis (ty - y))
    axis d | d > 2 = 1
           | d < -2 = -1
           | otherwise = 0 :: Int
    inAlcove = y <= 120 && x >= 302 && x <= 358
    alcove
      | frozen && y > 95 = hold (walk (0, -1))
      | mode == Normal && not longEast && long < 250 = walk (0, 1)
      | otherwise = noInput
    intoAlcove
      | x < 328 = (1, lane)
      | otherwise = (0, -1)
    -- The shaft: stop its wheel near an end of its track, cross, and let go on
    -- the far side. That debt is then paid on the move.
    crossShaft
      | frozen = hold (walk (1, lane))
      | x >= -446 = walk (1, lane)
      | mode == Normal && abs (shaftY - 166) >= 72 && longEast = hold (walk (1, lane))
      | otherwise = noInput

-- | The inputs that clear one floor, given its place in the campaign.
floorInputs :: Config -> [Level] -> Int -> [TickInput]
floorInputs cfg levels i = case i of
  0 -> drop 1 (expandScript undercroftScript)
  1 -> botInputs cfg level (shaftBot cfg level 26 [(-175, 0), (250, 1)])
  2 -> botInputs cfg level (shaftBot cfg level 24 [(-310, 0), (0, 1), (310, 2)])
  3 -> drop 1 solutionInputs
  4 -> botInputs cfg level (mainspringBot level)
  _ -> []
  where level = levels !! i

-- | Enter past the title, then for each floor: Enter past its note, play it, and
-- Enter past its victory banner. Ends on the ending screen.
campaignInputs :: Config -> [Level] -> [TickInput]
campaignInputs cfg levels = confirm : concatMap floorRun [0 .. length levels - 1]
  where
    confirm = noInput { tiConfirmPressed = True }
    floorRun i = confirm : floorInputs cfg levels i ++ [confirm]
