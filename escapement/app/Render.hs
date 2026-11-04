-- | Draws the game. Reads 'Campaign' and 'Fx'; changes neither.
module Render
  ( Scene
  , mkScene
  , render
  ) where

import Art
import Game.Campaign
import Game.Fx
import Game.Level
import Game.Story
import Game.TimeDebt (canFreeze)
import Game.Types
import Graphics.Gloss
import Text.Printf (printf)

-- | Geometry built once per floor, so a frame only pays for drawing it.
data Scene = Scene
  { scBed        :: Picture  -- ^ machine plate
  , scFloors     :: Picture  -- ^ walkable space, with its outline
  , scFloorShape :: Picture  -- ^ the same floors, uncoloured, for mode tints
  , scRails      :: [Picture]
  , scDial       :: Picture
  , scFrostEdge  :: Picture
  , scGears      :: [(Float, Float, Float, Picture)]  -- ^ x, y, turn rate, cog
  , scWheel      :: Picture
  , scWheelCold  :: Picture
  , scExitMark   :: Picture
  , scModeHud    :: Picture
  , scTitle      :: Picture
  , scBrief      :: Picture
  , scReduced    :: Bool
  }

v2 :: Vec -> (Float, Float)
v2 (Vec x y) = (realToFrac x, realToFrac y)

rectF :: Rect -> (Float, Float, Float, Float)
rectF (Rect l b r t) = (realToFrac l, realToFrac b, realToFrac r, realToFrac t)

rectPic :: Float -> Rect -> Picture
rectPic grow rect =
  let (l, b, r, t) = rectF rect
  in quad (l - grow) (b - grow) (r + grow) (t + grow)

mkScene :: Config -> Int -> Int -> Level -> Scene
mkScene cfg floorNumber floorCount level = Scene
  { scBed = bed
  , scFloors = floors
  , scFloorShape = Pictures (map (rectPic 0) (lvlFloors level))
  , scRails = map (railPic . hazardRoute) (lvlHazards level)
  , scDial = dialFace
  , scFrostEdge = frostEdge
  , scGears = gears
  , scWheel = escapeWheelPic ember emberDeep (blend 0.4 ember cream) 12 hazardR
  , scWheelCold = escapeWheelPic ember (blend 0.45 emberDeep frostDeep) frost 12 hazardR
  , scExitMark = exitMark
  , scModeHud = hudPlate
  , scTitle = titleCard
  , scBrief = briefCard
  , scReduced = cfgReducedEffects cfg
  }
  where
    hazardR = realToFrac (cfgHazardRadius cfg)
    bed = Pictures
      [ Color plate (quad (-640) (-360) 640 360)
      , Color (withA 0.5 plateHi) (Pictures
          [ quad x (-290) (x + 1) 360 | x <- [-640, -560 .. 640] ])
      , Color (withA 0.5 plateHi) (Pictures
          [ quad (-640) y 640 (y + 1) | y <- [-290, -210 .. 360] ])
      , Color brassDim (Pictures [ ringAt x y 3 1.4 | (x, y) <- bolts ])
      ]
    bolts = [ (x, y) | x <- [-622, 622], y <- [-272, 340] ]
    floorShapes grow = Pictures (map (rectPic grow) (lvlFloors level))
    floors = Pictures
      ( [ Color ink (floorShapes 9)
        , Color brassDim (floorShapes 3)
        , Color floorTone (floorShapes 0)
        , Color floorLine (Pictures (concatMap floorGrid (lvlFloors level)))
        , zoneTint (lvlStartArea level) brass 0.05
        , startMarks
        ] ++ map zoneMarks (lvlZones level))
    zoneTint rect c alpha = Color (withA alpha c) (rectPic 0 rect)
    floorGrid rect =
      let (l, b, r, t) = rectF rect
      in [ quad x b (x + 1) t | x <- steps l r ] ++ [ quad l y r (y + 1) | y <- steps b t ]
    steps lo hi = takeWhile (< hi - 20) [lo + 80, lo + 160 ..]
    -- Large rooms carry their name on the floor; small refuges carry it beneath.
    zoneMarks (Zone rect name sub) =
      let (l, b, r, t) = rectF rect
          cx = (l + r) / 2
          cy = (b + t) / 2
          large = r - l >= 120
      in if large
        then Pictures
          [ zoneTint rect verdigris 0.16
          , if null name then Blank else Pictures
              [ if r - l >= 240
                  then at cx cy (Color (withA 0.40 verdigris) (Pictures
                         [ ThickCircle 88 2, ThickCircle 58 1.5, tickRing 12 3 98 7 13 2 ]))
                  else Blank
              , at cx (cy + 12) (Color verdigris (label 19 name))
              , at cx (cy - 14) (Color (withA 0.9 verdigris) (label 11 sub))
              , Color verdigris (boxLines 2 (l + 8) (b + 8) (r - 8) (t - 8))
              ]
          ]
        else Pictures
          [ zoneTint rect verdigris 0.26
          , Color verdigris (quad l t r (t + 2))
          , at cx (b - 16) (Color verdigris (label 9 name))
          , Color (withA 0.6 verdigris) (Pictures
              [ thickSeg 2 (x, t - 10) (x + 8, t - 2) | x <- [l + 4, l + 16 .. r - 12] ])
          ]
    -- An open room has no one way forward, so it gets no arrows or start box.
    startMarks | not (null (lvlPins level)) = Blank
               | otherwise =
      let (l, b, r, t) = rectF (lvlStartArea level)
          (sx, sy) = v2 (lvlStart level)
      in Pictures
        [ Color (withA 0.8 brass) (boxLines 2 (l + 8) (b + 8) (r - 8) (t - 8))
        , at ((l + r) / 2) (b + 26) (Color brass (label 19 "START"))
        , Color (withA 0.55 brass) (Pictures
            [ at x sy (chevron 3 9)
            | x <- takeWhile (< r - 12) [sx + 45, sx + 72, sx + 99] ])
        ]
    railPic route =
      let points = map v2 (routePoints route)
          (a, b) = (head points, last points)
          across = if fst a == fst b then (1, 0) else (0, 1)
          offset k (x, y) = (x + k * fst across, y + k * snd across)
          count = max 2 (floor (sqrt ((fst b - fst a) ^ (2 :: Int) + (snd b - snd a) ^ (2 :: Int)) / 40)) :: Int
          lerpTo u = (fst a + (fst b - fst a) * u, snd a + (snd b - snd a) * u)
      in Pictures
        [ Color (withA 0.55 brassDim) (Pictures
            [ thickSeg 2 (offset (-9) a) (offset (-9) b)
            , thickSeg 2 (offset 9 a) (offset 9 b) ])
        , Color (withA 0.4 brassDim) (Pictures
            [ thickSeg 2 (offset (-9) p) (offset 9 p)
            | i <- [0 .. count]
            , let p = lerpTo (fromIntegral i / fromIntegral count) ])
        , Color brass (Pictures
            [ thickSeg 3 (offset (-13) p) (offset 13 p) | p <- [a, b] ])
        ]
    dialFace = Pictures
      [ Color plateHi (circleSolid 30)
      , Color ink (circleSolid 26)
      , Color brass (ThickCircle 26 2)
      , Color brass (tickRing 12 3 22 3 5 1.5)
      ]
    frostEdge = Pictures
      [ at x y (Rotate rot corner)
      | (x, y, rot) <- [ (-632, 352, 0), (632, 352, 90), (632, -282, 180), (-632, -282, 270) ] ]
    corner = Pictures
      [ thickSeg 3 (0, 0) (70, 0), thickSeg 3 (0, 0) (0, -70)
      , thickSeg 2 (10, 0) (10, -14), thickSeg 2 (0, -10) (14, -10)
      , thickSeg 2 (26, 0) (26, -10), thickSeg 2 (0, -26) (10, -26) ]
    gears =
      [ (realToFrac x, realToFrac y, realToFrac rate, gearPic teeth (realToFrac outer) (realToFrac inner))
      | (x, y, rate, teeth, outer, inner) <- lvlCogs level ]
    exitMark = Pictures
      [ Color verdigris (ThickCircle 34 3)
      , Color (withA 0.55 verdigris) (ThickCircle 24 2)
      , Color (withA 0.35 verdigris) (tickRing 12 3 46 5 9 2)
      ]
    hudPlate = Pictures
      [ Color ink (quad (-640) (-360) 640 (-290))
      , Color brassDim (quad (-640) (-290) 640 (-288))
      ]
    titleCard = Pictures
      ( [ Color (withA 0.88 ink) (quad (-640) (-360) 640 360)
        , at 0 40 (Color (withA 0.16 brass) (tickRing 36 3 330 8 22 3))
        , at 0 40 (Color (withA 0.10 brass) (ThickCircle 330 3))
        , at 0 120 (Color brass (label 74 gameTitle))
        ]
        ++ [ at 0 (58 - 30 * fromIntegral i) (Color cream (label 17 line))
           | (i, line) <- zip [0 :: Int ..] gameTagline ]
        ++ [ at 0 (-104) (Color cream (label 14 "WASD OR ARROWS MOVE     HOLD SPACE BRAKE"))
           , at 0 (-132) (Color cream (label 14 "P PAUSE     R RESTART     ESC QUIT"))
           ])
    briefCard = Pictures
      ( [ Color (withA 0.94 ink) (quad (-640) (-360) 640 360)
        , at 0 40 (Color (withA 0.10 brass) (tickRing 36 3 330 8 22 3))
        , at 0 232 (Color brassDim (label 15 (printf "FLOOR %d OF %d" floorNumber floorCount)))
        , at 0 165 (Color brass (label 40 (lvlTitle level)))
        , Color brassDim (quad (-300) 142 300 144)
        ]
        ++ [ at 0 (95 - 40 * fromIntegral i) (Color cream (label 17 line))
           | (i, line) <- zip [0 :: Int ..] (lvlBrief level) ])

-- | Mode icons: shape alone tells the modes apart.
normalIcon, frozenIcon, repayIcon :: Float -> Picture
normalIcon s = Polygon [(-s * 0.6, -s * 0.8), (s * 0.8, 0), (-s * 0.6, s * 0.8)]
frozenIcon s = Pictures
  [ quad (-s * 0.75) (-s * 0.8) (-s * 0.2) (s * 0.8), quad (s * 0.2) (-s * 0.8) (s * 0.75) (s * 0.8) ]
repayIcon s = Pictures
  [ Polygon [(-s, -s * 0.8), (0, 0), (-s, s * 0.8)], Polygon [(0, -s * 0.8), (s, 0), (0, s * 0.8)] ]

-- | The window keeps the 16:9 picture: scale to fit, then cover the bars.
render :: Config -> [Level] -> [Scene] -> Campaign -> Fx -> (Int, Int) -> Picture
render cfg levels scenes campaign fx (winW, winH) =
  let w = fromIntegral winW :: Float
      h = fromIntegral winH :: Float
      k = min (w / 1280) (h / 720)
      barX = (w - 1280 * k) / 2
      barY = (h - 720 * k) / 2
      bars = Color black (Pictures
        [ quad (-w / 2) (-h / 2) (-w / 2 + barX) (h / 2)
        , quad (w / 2 - barX) (-h / 2) (w / 2) (h / 2)
        , quad (-w / 2) (-h / 2) (w / 2) (-h / 2 + barY)
        , quad (-w / 2) (h / 2 - barY) (w / 2) (h / 2) ])
      index = cIndex campaign
      picture = screen cfg (levels !! index) (scenes !! index) campaign fx
  in Pictures [Scale k k picture, bars]

screen :: Config -> Level -> Scene -> Campaign -> Fx -> Picture
screen cfg level scene campaign fx = case cScreen campaign of
  OnTitle -> Pictures
    [ scBed scene, scTitle scene
    , at 0 (-228) (Color (pulseBrass 0.06) (label 24 "PRESS ENTER"))
    , if scReduced scene then Blank
        else at 0 40 (Color (withA 0.3 brass) (Rotate (tick * 2) (thickSeg 3 (0, 0) (0, 320))))
    ]
  OnBrief -> Pictures
    [ scBed scene, scBrief scene
    , at 0 (-228) (Color (pulseBrass 0.06) (label 20 "PRESS ENTER"))
    ]
  OnEnding -> Pictures
    ( [ scBed scene
      , Color (withA 0.94 ink) (quad (-640) (-360) 640 360)
      , at 0 40 (Color (withA 0.14 verdigris) (tickRing 36 3 330 8 22 3))
      , at 0 200 (Color verdigris (label 46 (head endingLines)))
      ]
      ++ [ at 0 (130 - 42 * fromIntegral i) (Color cream (label 17 line))
         | (i, line) <- zip [0 :: Int ..] (drop 1 endingLines) ]
      ++ [ at 0 20 (Color brass (label 15 endingSignOff))
         , at 0 (-70) (Color cream (label 15 (printf "OUT IN %.1f S.   BORROWED %.1f S.   CAUGHT %d TIMES."
             (secondsOf (cTotalTicks campaign)) (secondsOf (cBorrowed campaign)) (cTotalDeaths campaign))))
         , at 0 (-228) (Color (pulseBrass 0.06) (label 20 "ENTER TO BEGIN AGAIN"))
         ])
  InFloor -> floorFrame cfg level scene campaign fx
  where
    tick = fromIntegral (fxTick fx) :: Float
    pulseBrass rate = withA (0.75 + 0.25 * sin (tick * rate)) brass
    secondsOf n = fromIntegral n / fromIntegral (cfgTicksPerSecond cfg) :: Double

floorFrame :: Config -> Level -> Scene -> Campaign -> Fx -> Picture
floorFrame cfg level scene campaign fx = Pictures
  [ scBed scene
  , gearsLayer
  , scFloors scene
  , Color (withA (0.34 * frostF) frostDeep) (scFloorShape scene)
  , Color (withA (0.13 * strainF) ember) (scFloorShape scene)
  , Pictures (scRails scene)
  , repayStreaks
  , pinsPic
  , exitPad
  , notePanel
  , gatesPic
  , trails
  , ghosts
  , Pictures (zipWith hazardPic (lvlHazardsPositions) [0 :: Int ..])
  , playerPic
  , sparks
  , rings
  , Color (withA (0.9 * frostF) frost) (scFrostEdge scene)
  , strainEdges
  , promptPic
  , hud
  , overlay
  ]
  where
    game = cGame campaign
    time = gTime game
    mode = tsMode time
    clock = fromIntegral (tsClock time) :: Float
    frostF = realToFrac (fxFrost fx) :: Float
    strainF = realToFrac (fxStrain fx) :: Float
    meterF = realToFrac (fxMeter fx) :: Float
    tick = fromIntegral (fxTick fx) :: Float
    reduced = scReduced scene
    moving = not reduced
    lvlHazardsPositions = hazardPositions level (tsClock time)
    nearCap = tsDebt time >= cfgMaxDebt cfg - 45 && mode == Frozen
    -- The machine stays quiet behind the play: tinted, but well below the wheels.
    machineTint = blend strainF (blend frostF (blend 0.5 plateHi brassDim) (blend 0.45 plateHi frostDeep))
                    (blend 0.42 plateHi emberDeep)

    gearsLayer = Color machineTint (Pictures
      [ at x y (Rotate (clock * rate) cog) | (x, y, rate, cog) <- scGears scene ])

    -- Streaks along every rail while repaying, moving with each wheel.
    repayStreaks
      | strainF < 0.02 || reduced = Blank
      | otherwise = Color (withA (0.65 * strainF) ember) (Pictures (map streak (lvlHazards level)))
    streak (Hazard route) =
      let Vec x0 y0 = routePosition route (tsClock time)
          Vec x1 y1 = routePosition route (tsClock time + 1)
          (ax, ay) = v2 (head (routePoints route))
          (bx, by) = v2 (last (routePoints route))
          len = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
          forward = (x1 - x0) * realToFrac (bx - ax) + (y1 - y0) * realToFrac (by - ay) >= 0
          u = (bx - ax) / len
          v = (by - ay) / len
          angle = negate (atan2 v u) * 180 / pi + (if forward then 0 else 180)
          shift = fromIntegral ((fxTick fx * 7) `mod` 60) :: Float
          travel d = if forward then d + shift else len - d - shift
      in Pictures
           [ at (ax + u * t) (ay + v * t) (Rotate angle (chevron 3 7))
           | d <- [0, 60 .. len - 60]
           , let t = travel d
           , t >= 0, t <= len ]

    pinsLeft = length (lvlPins level) - length (gPins game)

    exitPad =
      let (l, b, r, t) = rectF (lvlExit level)
          cx = (l + r) / 2
          cy = (b + t) / 2
          pulse = if moving then 1 + 0.04 * sin (tick * 0.05) else 1
      in at cx cy $
           if pinsLeft > 0
             then Pictures
               [ Color (withA 0.25 brassDim) (circleSolid 34)
               , Color brassDim (ThickCircle 34 3)
               , at 0 (-7) (Color brassDim (label 15 "EXIT"))
               , at 0 (-26) (Color brass (label 10 (printf "%d LEFT" pinsLeft)))
               ]
             else Pictures
               [ glow verdigris 38 0.55
               , Color (withA 0.25 verdigris) (circleSolid 34)
               , Scale pulse pulse (Rotate (tick * 0.3) (scExitMark scene))
               , at 0 (-7) (Color cream (label 15 "EXIT"))
               ]

    -- Pins: a bright diamond while it is there, a hollow socket once it is taken.
    pinsPic = Pictures
      [ at x y (if i `elem` gPins game
          then Color (withA 0.6 brassDim) (Pictures [ThickCircle 11 2])
          else Pictures
            [ glow brass 14 (0.55 + 0.2 * sin (tick * 0.07))
            , Color brass (diamond 12)
            , Color cream (diamond 5)
            ])
      | (i, pin) <- zip [0 :: Int ..] (lvlPins level)
      , let (x, y) = v2 pin ]

    -- Doors. Shut: solid, striped iron, drawn exactly to its blocking rectangle.
    -- Open: bare floor between two rails. Lamps along it count down to the change.
    gatesPic = Pictures (map gatePic (lvlGates level))
    gatePic gate =
      let (l, b, r, t) = rectF (gateRect gate)
          open = gateOpen gate (tsClock time)
          vertical = t - b > r - l
          phase = (tsClock time + gatePhase gate) `mod` gatePeriod gate
          left = if open then gateOpenFor gate - phase else gatePeriod gate - phase
          span' = if open then gateOpenFor gate else gatePeriod gate - gateOpenFor gate
          lamps = 5 :: Int
          lit = ceiling (fromIntegral lamps * fromIntegral left / fromIntegral span' :: Float) :: Int
          c = if open then verdigris else ember
          lampAt i =
            let u = (fromIntegral i + 0.5) / fromIntegral lamps :: Float
            in if vertical then (l - 9, b + (t - b) * u) else (l + (r - l) * u, b - 9)
          stripes
            | vertical = [ thickSeg 3 (l, y) (r, y + 8) | y <- [b + 4, b + 16 .. t - 8] ]
            | otherwise = [ thickSeg 3 (x, b) (x + 8, t) | x <- [l + 4, l + 16 .. r - 8] ]
      in Pictures
           ( (if open
                then [ Color verdigris (if vertical
                         then Pictures [quad l (b - 3) r b, quad l t r (t + 3)]
                         else Pictures [quad (l - 3) b l t, quad r b (r + 3) t]) ]
                else [ Color ink (quad l b r t)
                     , Color brass (boxLines 2 l b r t)
                     , Color (withA 0.8 ember) (Pictures stripes) ])
           ++ [ let (x, y) = lampAt i
                in at x y (Color (if i < lit then c else withA 0.25 brassDim) (diamond 3.5))
              | i <- [0 .. lamps - 1] ])

    -- What each door will be when the debt clears: a small word beside it.
    gateGhosts =
      [ at ((l + r) / 2) (t + 12) (Color (if willOpen then verdigris else ember)
          (label 9 (if willOpen then "WILL OPEN" else "WILL SHUT")))
      | gate <- lvlGates level
      , let (l, _, r, t) = rectF (gateRect gate)
            willOpen = gateOpen gate (tsClock time + 2 * tsDebt time) ]

    trails = Pictures
      [ Color (withA (0.34 * (1 - fromIntegral i / fromIntegral (cfgTrailLength cfg)) * max 0.2 strainF) ember)
          (at x y (circleSolid (realToFrac (cfgHazardRadius cfg) * (0.86 - 0.04 * fromIntegral i))))
      | samples <- fxTrails fx
      , (i, p) <- zip [0 :: Int ..] samples
      , i `mod` 2 == 1
      , let (x, y) = v2 p ]

    -- Where each wheel lands when the debt clears: the cost, drawn in the world.
    ghosts
      | not (cfgGhost cfg && lvlGhosts level) || tsDebt time == 0 || gPhase game /= Playing = Blank
      | otherwise =
          let c = if mode == Frozen then frost else ember
              r = realToFrac (cfgHazardRadius cfg) :: Float
          in Pictures
               ( gateGhosts ++
               [ at x y (Pictures
                   [ Color (withA 0.16 c) (circleSolid r)
                   , Color (withA 0.85 c) (ThickCircle r 2)
                   , Color (withA 0.85 c) (Pictures [ Rotate a (at 0 r (quad (-3) (-5) 3 5)) | a <- [0, 45 .. 315] ])
                   ])
               | p <- settledPositions level time
               , let (x, y) = v2 p ])

    hazardPic pos i =
      let (x, y) = v2 pos
          route = hazardRoute (lvlHazards level !! i)
          period = fromIntegral (routePeriod route) :: Float
          spin = negate (clock * 720 / period) * (if period < 60 then 0.5 else 1)
          r = realToFrac (cfgHazardRadius cfg) :: Float
      in at x y (Pictures
           [ glow (blend frostF ember frost) r (0.5 + 0.25 * strainF)
           , Rotate spin (if frostF > 0.5 then scWheelCold scene else scWheel scene)
           , if frostF > 0.02
               then Color (withA frostF frost) (Pictures
                      [ hexOutline (r + 9) 3
                      , Pictures [ Rotate a (at 0 (r + 9) (quad (-5) (-5) 5 5)) | a <- [0, 120, 240] ] ])
               else Blank
           , if strainF > 0.3
               then Color (withA (0.8 * strainF) ember) (ThickCircle (r + 7) 2)
               else Blank
           ])

    playerPic =
      let (x, y) = v2 (gPlayer game)
          r = realToFrac (cfgPlayerRadius cfg) :: Float
          angle = negate (realToFrac (fxHeading fx)) * 180 / pi
          stretch = realToFrac (fxMotion fx) * 0.1 :: Float
          debtFrac = fromIntegral (tsDebt time) / fromIntegral (cfgMaxDebt cfg) :: Float
          arcColor = if mode == Frozen then frost else ember
          capPulse = 0.5 + 0.5 * sin (tick * 0.4)
      in at x y (Pictures
           [ glow cream r 0.55
           , Rotate angle (Scale (1 + stretch) (1 - stretch) (Pictures
               [ Color cream (circleSolid r)
               , Color brass (ThickCircle (r - 1) 2)
               , Color ink (at (r * 0.42) 0 (Polygon [(-3.5, -5), (5, 0), (-3.5, 5)]))
               ]))
           , if tsDebt time > 0
               then Pictures
                 [ Color (withA 0.35 ink) (ThickCircle (r + 8) 5)
                 , Color arcColor (Rotate (-90) (ThickArc 0 (360 * debtFrac) (r + 8) 5))
                 ]
               else Blank
           -- The brake is locked: the same cross as the panel, beside the player.
           , if mode == Repaying
               then at (r + 20) (r + 12) (Color ember (Pictures
                      [ thickSeg 2.5 (-5, -5) (5, 5), thickSeg 2.5 (-5, 5) (5, -5) ]))
               else Blank
           , if nearCap
               then Color (withA (0.45 + 0.55 * capPulse) ember) (ThickCircle (r + 16 + 3 * capPulse) 3)
               else Blank
           ])

    sparks = Pictures
      [ Color (withA (1 - fromIntegral (ptAge p) / fromIntegral (ptLife p)) (if ptCool p then verdigris else ember))
          (at x y (Rotate (fromIntegral (ptAge p) * 9) (quad (-2.2) (-2.2) 2.2 2.2)))
      | p <- fxParticles fx
      , let (x, y) = v2 (ptPos p) ]

    rings = Pictures
      [ Color (withA ((1 - u) * 0.9) c) (at x y (ThickCircle radius (3 + 4 * (1 - u))))
      | ring <- fxRings fx
      , let u = fromIntegral (rgAge ring) / fromIntegral (rgLife ring) :: Float
            (x, y) = v2 (rgPos ring)
            (c, reachPx) = case rgKind ring of
              RingFreeze  -> (frost, 180)
              RingRelease -> (ember, 120)
              RingCleared -> (verdigris, 110)
              RingPin     -> (brass, 90)
              RingImpact  -> (ember, 70)
              RingWin     -> (verdigris, 150)
            radius = 16 + reachPx * (1 - (1 - u) * (1 - u)) ]

    -- Ticks running along the top and sides while repaying. Motion and shape,
    -- never a flash.
    strainEdges
      | strainF < 0.02 = Blank
      | otherwise =
          let offset = if moving then fromIntegral ((fxTick fx * 6) `mod` 40) else 0
              c = withA (0.75 * strainF) ember
          in Color c (Pictures $
               [ at (x + offset) 351 (chevron 3 5) | x <- [-640, -600 .. 600] ]
               ++ [ at (x + offset) (-280) (chevron 3 5) | x <- [-640, -600 .. 600] ])

    -- Teaching beside the player, so the eye stays on the action. A forced release
    -- is warned about on every floor; everything else only where a floor teaches it.
    promptPic
      | gPhase game /= Playing = Blank
      | nearCap = promptAt "CAP NEAR: LET GO"
      | not (cfgHints cfg) = Blank
      | otherwise = maybe Blank promptAt (activePrompt level game)
    promptAt line =
      let (x, y) = v2 (gPlayer game)
          w = labelWidth 12.5 line
          px = max (w / 2 - 620) (min (620 - w / 2) x)
          py = min 340 (y + 38)
      in at px py (Pictures
           [ Color (withA 0.82 ink) (quad (-w / 2 - 8) (-6) (w / 2 + 8) 19)
           , Color modeColor (label 12.5 line)
           ])

    -- The floor's name, with a hint only once the player has failed here twice.
    notePanel =
      let (x, y) = v2 (lvlNoteAt level)
          hintLines = if cfgHints cfg && hintsDue campaign then lvlHint level else []
      in at x y (Pictures
           ( Color brassDim (at 0 (-8) (labelL 11 (lvlTitle level)))
           : [ Color (if i == 0 then modeColor else withA 0.85 cream) (at 0 (-32 - 22 * fromIntegral i) (labelL 12 line))
             | (i, line) <- zip [0 :: Int ..] hintLines ]))
    modeColor = case mode of
      Normal   -> brass
      Frozen   -> frost
      Repaying -> ember

    hud = Pictures
      [ scModeHud scene
      , modePlate
      , dial
      , meter
      , debtPlate
      ]

    -- The hazard clock, one turn per 180 ticks, shrunk into the panel.
    dial = at (-322) (-325) (Pictures
      [ scDial scene
      , Color (blend strainF (blend frostF brass frost) ember)
          (Rotate (clock * 2) (thickSeg 3 (0, -4) (0, 20)))
      , Color cream (circleSolid 3)
      ])

    modePlate =
      let icon = case mode of
            Normal   -> normalIcon 18
            Frozen   -> frozenIcon 18
            Repaying -> repayIcon 18
          name = case mode of
            Normal   -> "NORMAL"
            Frozen   -> "FROZEN"
            Repaying -> "REPAYING"
          sub = case mode of
            Normal   -> "WHEELS AT 1X"
            Frozen   -> "STOPPED, STILL LETHAL"
            Repaying -> "WHEELS AT 2X"
      in Pictures
           [ at (-606) (-325) (Color modeColor icon)
           , at (-578) (-327) (Color modeColor (labelL 24 name))
           , at (-578) (-349) (Color (withA 0.8 cream) (labelL 9.5 sub))
           ]

    meter =
      let l = -250 :: Float
          r = 250 :: Float
          b = -335 :: Float
          t = -309 :: Float
          fill = l + (r - l) * clamp01 meterF
          exact = l + (r - l) * (fromIntegral (tsDebt time) / fromIntegral (cfgMaxDebt cfg))
          fillC = if mode == Repaying then ember else frost
          stripes
            | mode /= Repaying || tsDebt time == 0 = Blank
            | otherwise =
                let phase = if moving then fromIntegral ((fxTick fx * 2) `mod` 22) else 0
                in Color (withA 0.55 emberDeep) (Pictures
                     [ thickSeg 6 (x, b + 1) (x + 12, t - 1)
                     | x <- takeWhile (< fill - 14) [l + 4 - phase + 22, l + 26 - phase + 22 ..]
                     , x > l ])
          notch x = Color brass (quad (x - 1) (b - 4) (x + 1) (t + 2))
      in Pictures
           [ Color plate (quad l b r t)
           , Color fillC (quad l b fill t)
           , stripes
           , if tsDebt time > 0 then Color cream (quad (exact - 1.5) (b - 2) (exact + 1.5) (t + 2)) else Blank
           , Color brass (boxLines 2 (l - 2) (b - 2) (r + 2) (t + 2))
           , notch (l + (r - l) / 3)
           , notch (l + (r - l) * 2 / 3)
           , Color (withA 0.8 cream) (Pictures
               [ at x (-353) (label 9.5 s)
               | (x, s) <- [(l, "0"), (l + (r - l) / 3, "1"), (l + (r - l) * 2 / 3, "2"), (r, "3 S")] ])
           , at l (-303) (Color brassDim (labelL 9.5 "TIME DEBT"))
           ]

    debtPlate =
      let seconds = fromIntegral (tsDebt time) / fromIntegral (cfgTicksPerSecond cfg) :: Double
          ready = canFreeze time
          (status, statusC) = case mode of
            Normal   -> ("BRAKE READY", verdigris)
            Frozen   -> ("BRAKE HELD", frost)
            Repaying -> ("BRAKE LOCKED", ember)
          lamp | ready     = Color verdigris (diamond 8)
               | mode == Frozen = Color frost (diamond 8)
               | otherwise = Color ember (Pictures [ thickSeg 2 (-6, -6) (6, 6), thickSeg 2 (-6, 6) (6, -6) ])
      in Pictures
           [ at 282 (-333) (Color (withA 0.85 cream) (labelL 12 "DEBT"))
           , at 332 (-333) (Color modeColor (labelL 24 (printf "%.1f S" seconds)))
           , at 462 (-321) lamp
           , at 478 (-327) (Color statusC (labelL 11 status))
           , at 478 (-347) (Color (withA 0.7 cream) (labelL 9 "P PAUSE  R RESTART"))
           ]

    overlay = case gPhase game of
      Paused -> banner frost "PAUSED" "P TO RESUME    R TO RESTART"
        (if mode == Frozen && tsDebt time > 0
           then "A BRAKE DOES NOT SURVIVE A PAUSE: THE DEBT FALLS DUE ON RESUME."
           else "THE CLOCK AND THE DEBT ARE HOLDING.")
      Lost -> banner ember "CAUGHT" "ENTER OR R TO RETRY"
        (maybe "THE DOOR SHUT ON YOU. READ THE LAMPS BESIDE IT." (const (lossReason mode)) (gHitBy game))
      Won -> banner verdigris (ratingOr (lvlTitle level)) "ENTER TO CLIMB ON   R TO TRY AGAIN"
        (printf "CLEARED IN %.1f S, HAVING BORROWED %.1f S.%s"
           (secondsOf (gTicks game)) (secondsOf (gBorrowed game)) parNote)
      _ -> Blank
    secondsOf n = fromIntegral n / fromIntegral (cfgTicksPerSecond cfg) :: Double

    -- The less borrowed, the better the rating. Floors without a par are unrated.
    par = lvlPar level
    ratingOr fallback
      | par == 0 = fallback
      | gBorrowed game <= par = "PAID IN FULL"
      | gBorrowed game * 2 <= par * 3 = "SETTLED"
      | otherwise = "IN ARREARS"
    parNote :: String
    parNote
      | par == 0 = ""
      | otherwise = printf " PAID IN FULL: %.1f S." (secondsOf par)

    -- A band, not a full-screen wash, so the wheel that got you stays visible.
    banner c big small detail =
      let eased = clamp01 (fromIntegral (fxScreenAge fx) / 14)
          h = 96 * (1 - (1 - eased) * (1 - eased))
      in at 0 (-205) (Pictures
           [ Color (withA 0.93 ink) (quad (-640) (-h) 640 h)
           , Color c (quad (-640) (h - 2) 640 h)
           , Color c (quad (-640) (-h) 640 (2 - h))
           , if eased > 0.6
               then Pictures
                 [ at 0 18 (Color c (label 46 big))
                 , at 0 (-14) (Color cream (label 13.5 detail))
                 , at 0 (-46) (Color (withA (0.75 + 0.25 * sin (tick * 0.08)) brass) (label 17 small))
                 ]
               else Blank
           ])

lossReason :: TemporalMode -> String
lossReason mode = case mode of
  Normal   -> "THE WHEEL FOUND YOU. HOLD SPACE TO STOP IT FIRST."
  Frozen   -> "A STOPPED WHEEL STILL CUTS. WALK AROUND IT."
  Repaying -> "THE DEBT CAME DUE. WATCH WHERE THE GHOST LANDS BEFORE YOU LET GO."

clamp01 :: Float -> Float
clamp01 = max 0 . min 1
