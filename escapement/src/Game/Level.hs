-- | The floors of the tower, and where their hazards are at any clock tick.
module Game.Level
  ( mkLevel
  , mkLevels
  , mkRooms
  , gateOpen
  , closedGates
  , wallsFromFloors
  , safeZones
  , routePosition
  , routeLength
  , routePeriod
  , hazardPositions
  , settledPositions
  , levelCycle
  , activePrompt
  ) where

import Data.List (nub, sort)
import Game.Collision (rectContains)
import Game.Story
import Game.Types

-- | The campaign, in play order. Each floor adds one idea:
--
-- 1. Holding the brake stops a wheel.
-- 2. A stopped wheel still blocks: where you catch it matters.
-- 3. Letting go runs the clock double and locks the brake until the debt clears.
-- 4. A wheel in a corridor can only be passed from a refuge.
-- 5. One clock rules every wheel, and debt can be repaid on the move.
mkLevels :: Config -> [Level]
mkLevels cfg = [undercroft, shafts, countingRoom, mkLevel cfg, mainspring]

-- | Machine rooms: open floors with several routes. The sample comes first.
mkRooms :: Config -> [Level]
mkRooms _ = [escapeWheel]

-- | The sample machine room. A hub with three wings, one pin in each, taken in
-- any order, and each with its own price:
--
-- * West: a fast wheel in a slot. A short brake, cheap.
-- * East: a long corridor swept by one wheel, with an alcove at the far end. The
--   same corridor must be walked twice, so the price is largest here.
-- * South: two doors in opposite phase, with a fast wheel between them. The far
--   door can be held open with the brake, or caught as it swings.
--
-- >                 [ EXIT ]
-- >  PIN | slot | [   HUB   ] | ---- corridor ---- PIN alcove
-- >                 |door|
-- >              vestibule -- shaft --|door| VAULT, PIN
escapeWheel :: Level
escapeWheel = withNote floorEscapeWheel blankLevel
  { lvlWalls = wallsFromFloors floors
  , lvlFloors = floors
  , lvlStart = Vec 0 (-20)
  , lvlStartArea = hub
  , lvlZones =
      [ Zone westRoom "" ""
      , Zone alcove "" ""
      , Zone vestibule "" ""
      , Zone vault "" "" ]
  , lvlExit = Rect (-34) 140 34 200
  , lvlHazards =
      [ Hazard (Route [Vec (-180) 75, Vec (-180) (-25)] 15 0)
      , Hazard (Route [Vec 180 25, Vec 580 25] 40 0)
      , Hazard (Route [Vec 176 (-120), Vec 176 (-220)] 15 0)
      ]
  , lvlGates =
      [ Gate (Rect (-35) (-84) 35 (-70)) 120 40 0
      , Gate (Rect 300 (-200) 314 (-140)) 120 40 60
      ]
  , lvlPins = [Vec (-340) 25, Vec 460 (-52), Vec 430 (-170)]
  , lvlPar = 460
  , lvlNoteAt = Vec (-576) 330
  , lvlCogs = defaultCogs
  }
  where
    hub = Rect (-120) (-70) 120 110
    exitBay = Rect (-40) 110 40 210
    westRoom = Rect (-420) (-70) (-240) 120
    alcove = Rect 432 (-79) 488 (-21)
    vestibule = Rect (-80) (-216) 60 (-84)
    vault = Rect 314 (-250) 500 (-100)
    floors =
      [ hub, exitBay
      , Rect (-134) (-10) (-120) 60, Rect (-226) (-55) (-134) 105
      , Rect (-240) (-10) (-226) 60, westRoom
      , Rect 120 (-10) 134 60, Rect 134 (-21) 620 71, alcove
      , Rect (-35) (-84) 35 (-70), vestibule
      , Rect 60 (-200) 130 (-140), Rect 130 (-250) 222 (-90), Rect 222 (-200) 300 (-140)
      , Rect 300 (-200) 314 (-140), vault
      ]

withNote :: NoteText -> Level -> Level
withNote note level = level
  { lvlTitle = noteTitle note, lvlBrief = noteBrief note, lvlHint = noteHint note }

-- | Everything inside the play area that is not floor becomes wall.
wallsFromFloors :: [Rect] -> [Rect]
wallsFromFloors floors = concatMap band (zip ys (drop 1 ys))
  where
    edges lo hi f = sort (nub ([lo, hi] ++ concatMap f floors))
    xs = edges (-640) 640 (\(Rect l _ r _) -> [l, r])
    ys = edges (-290) 360 (\(Rect _ b _ t) -> [b, t])
    band (b, t) = merge [ (l, r) | (l, r) <- zip xs (drop 1 xs), not (covered l b r t) ]
      where
        merge ((a, c) : (d, e) : rest)
          | c == d    = merge ((a, e) : rest)
          | otherwise = Rect a b c t : merge ((d, e) : rest)
        merge [(a, c)] = [Rect a b c t]
        merge []       = []
    covered l b r t = any (\(Rect fl fb fr ft) -> fl <= l && r <= fr && fb <= b && t <= ft) floors

-- | Floor 1: the first brake. Any stop can be walked around, and the room beyond
-- is long enough to watch the wheel run double before the exit.
undercroft :: Level
undercroft = withNote floorUndercroft blankLevel
  { lvlWalls = wallsFromFloors floors
  , lvlFloors = floors
  , lvlStart = Vec (-520) 70
  , lvlStartArea = startRoom
  , lvlZones = [Zone workshop "WORKSHOP" "NO WHEEL REACHES THIS ROOM"]
  , lvlExit = Rect 400 36 468 104
  , lvlHazards = [Hazard (Route [Vec (-340) 120, Vec (-340) 20] 15 0)]
  , lvlPrompts =
      [ Prompt (Just workshop) (Just Frozen) "LET GO OF SPACE"
      , Prompt (Just workshop) (Just Repaying) "THE CLOCK IS CATCHING UP"
      , Prompt Nothing (Just Frozen) "KEEP HOLDING. WALK AROUND IT"
      , Prompt (Just (Rect (-470) (-140) (-280) 200)) (Just Normal) "HOLD SPACE"
      , Prompt (Just startRoom) (Just Normal) "WASD OR ARROWS TO MOVE"
      ]
  , lvlNoteAt = Vec (-576) 300
  , lvlCogs = defaultCogs
  , lvlGhosts = False
  }
  where
    startRoom = Rect (-600) (-140) (-400) 200
    workshop = Rect (-280) (-140) 500 200
    floors =
      [ startRoom, Rect (-400) 35 (-386) 105, Rect (-386) (-10) (-294) 150
      , Rect (-294) 35 (-280) 105, workshop ]

-- | Geometry shared by floors 2 and 3: a run of rooms joined by a narrow
-- corridor, with a vertical shaft across each join. A wheel runs up and down
-- each shaft, so it blocks the corridor whenever it is caught near the middle.
shaftFloors :: Double -> Double -> [(Double, Double)] -> [Rect]
shaftFloors cy halfHeight = concatMap piece . pairs
  where
    pairs rooms = zip rooms (drop 1 rooms)
    piece ((_, roomEnd), (nextStart, _)) =
      let cx = (roomEnd + nextStart) / 2
      in [ Rect roomEnd (cy - 20) (cx - 38) (cy + 20)
         , Rect (cx - 38) (cy - halfHeight) (cx + 38) (cy + halfHeight)
         , Rect (cx + 38) (cy - 20) nextStart (cy + 20) ]

floorY :: Double
floorY = 35

roomsOf :: Double -> [(Double, Double)] -> [Rect]
roomsOf h rooms = [Rect l (floorY - h) r (floorY + h) | (l, r) <- rooms]

-- | Floor 2: stops in the middle of the track block the way, stops near the end
-- do not. No crossing is possible without the brake.
shafts :: Level
shafts = withNote floorShafts blankLevel
  { lvlWalls = wallsFromFloors floors
  , lvlFloors = floors
  , lvlStart = Vec (-520) floorY
  , lvlStartArea = roomOne
  , lvlZones =
      [ Zone (Rect (-30) (floorY - 120) 110 (floorY + 120)) "LANDING" ""
      , Zone (Rect 390 (floorY - 120) 600 (floorY + 120)) "" "" ]
  , lvlExit = Rect 490 (floorY - 34) 558 (floorY + 34)
  , lvlHazards = [shaftWheel (-175) 55 16 0, shaftWheel 250 55 16 5]
  , lvlPrompts =
      [ Prompt Nothing (Just Frozen) "A STOPPED WHEEL STILL CUTS"
      , Prompt (Just roomOne) (Just Normal) "CATCH IT AWAY FROM THE DOOR"
      ]
  , lvlNoteAt = Vec (-576) 300
  , lvlCogs = defaultCogs
  , lvlGhosts = False
  }
  where
    roomOne = Rect (-600) (floorY - 120) (-320) (floorY + 120)
    rooms = [(-600, -320), (-30, 110), (390, 600)]
    floors = roomsOf 120 rooms ++ shaftFloors floorY 101 rooms

shaftWheel :: Double -> Double -> Int -> Int -> Hazard
shaftWheel cx stroke oneWay phase =
  Hazard (Route [Vec cx (floorY + stroke), Vec cx (floorY - stroke)] oneWay phase)

-- | Floor 3: the wheels are a quarter-turn apart, so no moment suits two shafts
-- at once. Each crossing must be paid for before the next can begin.
countingRoom :: Level
countingRoom = withNote floorCountingRoom blankLevel
  { lvlWalls = wallsFromFloors floors
  , lvlFloors = floors
  , lvlStart = Vec (-540) floorY
  , lvlStartArea = roomOne
  , lvlZones =
      [ Zone (Rect (-180) (floorY - 60) (-120) (floorY + 60)) "" ""
      , Zone (Rect 120 (floorY - 60) 180 (floorY + 60)) "" ""
      , Zone (Rect 440 (floorY - 100) 600 (floorY + 100)) "" "" ]
  , lvlExit = Rect 490 (floorY - 34) 558 (floorY + 34)
  , lvlHazards = [shaftWheel (-310) 32 16 0, shaftWheel 0 32 16 8, shaftWheel 310 32 16 0]
  , lvlPrompts =
      [ Prompt Nothing (Just Repaying) "LOCKED UNTIL PAID. GHOSTS SHOW WHERE THE WHEELS LAND"
      , Prompt Nothing (Just Frozen) "LET GO AS SOON AS YOU ARE THROUGH"
      ]
  , lvlNoteAt = Vec (-576) 300
  , lvlCogs = defaultCogs
  }
  where
    roomOne = Rect (-600) (floorY - 100) (-440) (floorY + 100)
    rooms = [(-600, -440), (-180, -120), (120, 180), (440, 600)]
    floors =
      [ Rect (-600) (floorY - 100) (-440) (floorY + 100)
      , Rect (-180) (floorY - 60) (-120) (floorY + 60)
      , Rect 120 (floorY - 60) 180 (floorY + 60)
      , Rect 440 (floorY - 100) 600 (floorY + 100) ]
      ++ shaftFloors floorY 78 rooms

-- | Floor 5: a long wheel in the only corridor, and a shaft cutting across it.
-- The shaft's debt falls due in the open, so it is repaid on the move, with the
-- long wheel already coming back.
mainspring :: Level
mainspring = withNote floorMainspring blankLevel
  { lvlWalls = wallsFromFloors floors
  , lvlFloors = floors
  , lvlStart = Vec (-505) 40
  , lvlStartArea = startRoom
  , lvlZones =
      [ Zone alcove "REFUGE" ""
      , Zone springRoom "" "" ]
  , lvlExit = Rect 440 (-90) 560 (-10)
  , lvlHazards =
      [ Hazard (Route [Vec (-560) 166, Vec 570 166] 156 0)
      , Hazard (Route [Vec (-400) 270, Vec (-400) 62] 26 0)
      ]
  , lvlNoteAt = Vec (-200) 320
  , lvlCogs = defaultCogs
  }
  where
    startRoom = Rect (-600) (-140) (-460) 100
    gallery = Rect (-606) 120 616 212
    shaft = Rect (-446) 16 (-354) 316
    alcove = Rect 302 62 358 120
    springRoom = Rect 400 (-140) 600 100
    floors =
      [ startRoom, Rect (-540) 100 (-470) 120, gallery, shaft, alcove
      , Rect 440 100 510 120, springRoom ]

defaultCogs :: [Cog]
defaultCogs =
  [ (-575, -210, 1.6, 14, 50, 42)
  , (-8, -214, -1.2, 16, 66, 57)
  , (88, -234, 2.2, 9, 39, 31)
  , (540, -226, 0.8, 16, 62, 53)
  , (410, -210, -1.0, 18, 74, 64)
  , (-300, -224, 2.0, 9, 39, 31)
  ]

-- | Floor 4. Layout, left to right:
--
-- >                         +--- gallery (wheel 2 runs its full length) ---+
-- >                         ^ door              alcove         door v
-- > START | slot (wheel 1) | STILL ROOM                         EXIT chamber
--
-- Slot and gallery are 92 wide, narrower than a wheel and a player side by side,
-- so a frozen wheel still blocks them: where it stops matters. Neither has a
-- straight run long enough to walk by timing alone (see test/Probe.hs).
mkLevel :: Config -> Level
mkLevel cfg = withNote floorGallery blankLevel
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
  , lvlZones     =
      [ Zone stillRoom "STILL ROOM" "SAFE AT ANY SPEED"
      , Zone alcove "REFUGE" ""
      , Zone exitChamber "" "" ]
  , lvlExit      = Rect 501 36 569 104
  , lvlHazards   =
      [ Hazard (Route [Vec (-340) 120, Vec (-340) 20] (cfgShuttleOneWay cfg) (cfgShuttlePhase cfg))
      , Hazard (Route [Vec (-40) 260, Vec 570 260] (cfgGalleryOneWay cfg) (cfgGalleryPhase cfg))
      ]
  , lvlNoteAt = Vec (-576) 300
  , lvlCogs = (213, 30, -0.8, 28, 130, 112) : defaultCogs
  }
  where
    startRoom   = Rect (-600) (-140) (-400) 200
    slot        = Rect (-386) (-10) (-294) 150
    stillRoom   = Rect (-280) (-140) 30 200
    gallery     = Rect (-70) 214 600 306
    alcove      = Rect 396 156 452 214
    exitChamber = Rect 470 (-20) 600 200

safeZones :: Level -> [Rect]
safeZones = map zoneRect . lvlZones

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

-- | Where every wheel will be at the moment the debt clears, if the key is let go
-- now. Repaying moves the clock 2 per debt tick, so this point stays put for the
-- whole repayment, and it creeps forward only while the key is held.
settledPositions :: Level -> TimeState -> [Vec]
settledPositions level ts = hazardPositions level (tsClock ts + 2 * tsDebt ts)

-- | Only a function of the clock, like the wheels, so a door holds no state.
gateOpen :: Gate -> HazardTick -> Bool
gateOpen gate tick = (tick + gatePhase gate) `mod` gatePeriod gate < gateOpenFor gate

closedGates :: Level -> HazardTick -> [Rect]
closedGates level tick = [gateRect g | g <- lvlGates level, not (gateOpen g tick)]

-- | Clock ticks after which every wheel and door is back where it started.
levelCycle :: Level -> Int
levelCycle level =
  foldr lcm 1 (map (routePeriod . hazardRoute) (lvlHazards level) ++ map gatePeriod (lvlGates level))

-- | The teaching line for this moment, if any.
activePrompt :: Level -> Game -> Maybe String
activePrompt level game = case filter matches (lvlPrompts level) of
  (p : _) -> Just (promptText p)
  []      -> Nothing
  where
    matches p = maybe True (`rectContains` gPlayer game) (promptArea p)
             && maybe True (== tsMode (gTime game)) (promptMode p)
