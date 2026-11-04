-- | The one level, and where its hazards are at any clock tick.
module Game.Level
  ( mkLevel
  , routePosition
  , routeLength
  , routePeriod
  , hazardPositions
  , levelCycle
  ) where

import Game.Types

-- | Layout, left to right:
--
-- >                         +--- gallery (wheel 2 runs its full length) ---+
-- >                         ^ door              alcove         door v
-- > START | slot (wheel 1) | STILL ROOM                         EXIT chamber
--
-- Slot and gallery are 92 wide, narrower than a wheel and a player side by side,
-- so a frozen wheel still blocks them: where it stops matters. Neither has a
-- straight run long enough to walk by timing alone (see test/Probe.hs).
mkLevel :: Config -> Level
mkLevel cfg = Level
  { lvlWalls =
      [ Rect (-640) (-290) (-600) 360, Rect 600 (-290) 640 360     -- outer left, right
      , Rect (-600) 306 600 360, Rect (-600) (-290) 600 (-262)     -- outer top, bottom
      , Rect (-600) 200 (-70) 306                                  -- block above the rooms
      , Rect (-70) 200 (-50) 214, Rect 20 200 396 214              -- gallery south wall
      , Rect (-600) (-262) 600 (-140)                              -- block below the rooms
      , Rect (-400) (-140) (-386) 35, Rect (-400) 105 (-386) 200   -- start | slot
      , Rect (-294) (-140) (-280) 35, Rect (-294) 105 (-280) 200   -- slot | still room
      , Rect (-386) (-140) (-294) (-10), Rect (-386) 150 (-294) 200 -- slot end caps
      , Rect 30 (-140) 396 200                                     -- dial block
      , Rect 396 (-140) 470 156, Rect 452 156 470 214              -- around the alcove
      , Rect 470 (-140) 600 (-20)                                  -- below the exit chamber
      , Rect 470 200 500 214, Rect 570 200 600 214                 -- exit door jambs
      ]
  , lvlFloors =
      [ startRoom
      , Rect (-400) 35 (-386) 105      -- door
      , slot
      , Rect (-294) 35 (-280) 105      -- door
      , stillRoom
      , Rect (-50) 200 20 214          -- door
      , gallery
      , alcove
      , Rect 500 200 570 214           -- door
      , exitChamber
      ]
  , lvlStart     = Vec (-520) 70
  , lvlStartArea = startRoom
  , lvlSafeArea  = stillRoom
  , lvlRefuge    = alcove
  , lvlExit      = Rect 501 36 569 104
  , lvlHazards   =
      [ Hazard (Route [Vec (-340) 120, Vec (-340) 20] (cfgShuttleOneWay cfg) (cfgShuttlePhase cfg))
      , Hazard (Route [Vec (-40) 260, Vec 570 260] (cfgGalleryOneWay cfg) (cfgGalleryPhase cfg))
      ]
  }
  where
    startRoom   = Rect (-600) (-140) (-400) 200
    slot        = Rect (-386) (-10) (-294) 150
    stillRoom   = Rect (-280) (-140) 30 200
    gallery     = Rect (-70) 214 600 306
    alcove      = Rect 396 156 452 214
    exitChamber = Rect 470 (-20) 600 200

routeLength :: Route -> Double
routeLength route = sum (zipWith vDist points (drop 1 points))
  where points = routePoints route

routePeriod :: Route -> Int
routePeriod route = 2 * routeOneWay route

-- | Position at a hazard tick. Only a function of the clock, so hazards hold
-- no state that could drift, and continuity follows from the clock's own steps.
routePosition :: Route -> HazardTick -> Vec
routePosition route tick =
  let period = routePeriod route
      t = (tick + routePhase route) `mod` period
      leg = if t <= routeOneWay route then t else period - t
      distance = routeLength route * fromIntegral leg / fromIntegral (routeOneWay route)
  in walk distance (routePoints route)
  where
    walk _ [p] = p
    walk d (p : q : rest)
      | d <= segment = vLerp (d / segment) p q
      | otherwise    = walk (d - segment) (q : rest)
      where segment = vDist p q
    walk _ [] = Vec 0 0

hazardPositions :: Level -> HazardTick -> [Vec]
hazardPositions level tick = [routePosition (hazardRoute h) tick | h <- lvlHazards level]

-- | Clock ticks after which every hazard is back where it started.
levelCycle :: Level -> Int
levelCycle level = foldr (lcm . routePeriod . hazardRoute) 1 (lvlHazards level)
