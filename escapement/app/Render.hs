-- | Draws the game. Reads 'Game' and 'Fx'; changes neither.
module Render
  ( Scene
  , mkScene
  , render
  ) where

import Art
import Game.Fx
import Game.Level
import Game.TimeDebt (canFreeze)
import Game.Types
import Graphics.Gloss
import Text.Printf (printf)

-- | Geometry built once, so a frame only pays for drawing it.
data Scene = Scene
  { scBed        :: Picture  -- ^ machine plate and engraving
  , scFloors     :: Picture  -- ^ walkable space, with its outline
  , scFloorShape :: Picture  -- ^ the same floors, uncoloured, for mode tints
  , scRails      :: [Picture]
  , scDial       :: Picture
  , scFrostEdge  :: Picture
  , scGears      :: [(Float, Float, Float, Picture)]  -- ^ x, y, turn rate, cog
  , scWheel      :: Picture
  , scWheelCold  :: Picture
  , scExitMark   :: Picture
  , scChevrons   :: Picture
  , scModeHud    :: Picture
  , scTitle      :: Picture
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

mkScene :: Config -> Level -> Scene
mkScene cfg level = Scene
  { scBed = bed
  , scFloors = floors
  , scFloorShape = Pictures (map (rectPic 0) (lvlFloors level))
  , scRails = map (railPic . hazardRoute) (lvlHazards level)
  , scDial = dialFace
  , scFrostEdge = frostEdge
  , scGears = gears
  , scWheel = escapeWheelPic ember emberDeep (blend 0.4 ember cream) 12 hazardR
  , scWheelCold = escapeWheelPic (blend 0.25 ember cream) (blend 0.45 emberDeep frostDeep) frost 12 hazardR
  , scExitMark = exitMark
  , scChevrons = chevrons
  , scModeHud = hudPlate
  , scTitle = titleCard
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
      , at (-270) (-214) (Color (withA 0.85 brass) (label 40 "ESCAPEMENT"))
      , at (-270) (-246) (Color brassDim (label 12 "BORROW TIME. REPAY IT."))
      ]
    bolts = [ (x, y) | x <- [-622, 622], y <- [-272, 340] ]
    floorShapes grow = Pictures (map (rectPic grow) (lvlFloors level))
    floors = Pictures
      [ Color ink (floorShapes 9)
      , Color brassDim (floorShapes 3)
      , Color floorTone (floorShapes 0)
      , Color floorLine (Pictures (concatMap floorGrid (lvlFloors level)))
      , zoneTint (lvlStartArea level) brass 0.05
      , zoneTint (lvlSafeArea level) verdigris 0.16
      , zoneTint (lvlRefuge level) verdigris 0.26
      , zoneTint (Rect 470 (-20) 600 200) verdigris 0.13
      , stillRoomMarks
      , startMarks
      , refugeMarks
      ]
    zoneTint rect c alpha = Color (withA alpha c) (rectPic 0 rect)
    floorGrid rect =
      let (l, b, r, t) = rectF rect
      in [ quad x b (x + 1) t | x <- steps l r ] ++ [ quad l y r (y + 1) | y <- steps b t ]
    steps lo hi = takeWhile (< hi - 20) [lo + 80, lo + 160 ..]
    stillRoomMarks =
      let (l, b, r, t) = rectF (lvlSafeArea level)
          cx = (l + r) / 2
          cy = (b + t) / 2
      in Pictures
        [ at cx cy (Color (withA 0.40 verdigris) (Pictures
            [ ThickCircle 88 2, ThickCircle 58 1.5, tickRing 12 3 98 7 13 2 ]))
        , at cx (cy + 12) (Color verdigris (label 19 "STILL ROOM"))
        , at cx (cy - 14) (Color (withA 0.9 verdigris) (label 11 "SAFE AT ANY SPEED"))
        , Color verdigris (boxLines 2 (l + 8) (b + 8) (r - 8) (t - 8))
        ]
    startMarks =
      let (l, b, r, t) = rectF (lvlStartArea level)
      in Pictures
        [ Color (withA 0.8 brass) (boxLines 2 (l + 8) (b + 8) (r - 8) (t - 8))
        , at ((l + r) / 2) (b + 26) (Color brass (label 19 "START"))
        , Color (withA 0.55 brass) (Pictures
            [ at x 70 (chevron 3 9) | x <- [-475, -448, -421] ])
        ]
    refugeMarks =
      let (l, b, r, _) = rectF (lvlRefuge level)
      in Pictures
        [ Color verdigris (quad l 214 r 216)
        , at ((l + r) / 2) (b - 16) (Color verdigris (label 9 "REFUGE"))
        , Color (withA 0.6 verdigris) (Pictures
            [ thickSeg 2 (x, 204) (x + 8, 212) | x <- [l + 4, l + 16 .. r - 12] ])
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
    chevrons = Pictures [ at (fromIntegral i * 60) 0 (chevron 3 7) | i <- [0 .. 9 :: Int] ]
    dialFace = Pictures
      [ Color plateHi (circleSolid 150)
      , Color ink (circleSolid 136)
      , Color brass (ThickCircle 136 3)
      , Color brass (tickRing 36 3 130 6 13 2)
      , Color brassDim (ThickCircle 96 1.5)
      , Color brassDim (at 0 (-44) (label 10 "HAZARD CLOCK"))
      ]
    frostEdge = Pictures
      [ at x y (Rotate rot corner)
      | (x, y, rot) <- [ (-632, 352, 0), (632, 352, 90), (632, -282, 180), (-632, -282, 270) ] ]
    corner = Pictures
      [ thickSeg 3 (0, 0) (70, 0), thickSeg 3 (0, 0) (0, -70)
      , thickSeg 2 (10, 0) (10, -14), thickSeg 2 (0, -10) (14, -10)
      , thickSeg 2 (26, 0) (26, -10), thickSeg 2 (0, -26) (10, -26) ]
    gears =
      [ (-575, -210, 1.6, gearPic 14 50 42)
      , (-8, -214, -1.2, gearPic 16 66 57)
      , (88, -234, 2.2, gearPic 9 39 31)
      , (540, -86, 0.8, gearPic 16 62 53)
      , (410, -210, -1.0, gearPic 18 74 64)
      , (514, -222, 2.0, gearPic 9 39 31)
      ]
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
      [ Color (withA 0.88 ink) (quad (-640) (-360) 640 360)
      , at 0 40 (Color (withA 0.16 brass) (tickRing 36 3 330 8 22 3))
      , at 0 40 (Color (withA 0.10 brass) (ThickCircle 330 3))
      , at 0 214 (Color brass (label 74 "ESCAPEMENT"))
      , at 0 168 (Color cream (label 17 "YOU CAN STOP DANGER NOW,"))
      , at 0 140 (Color cream (label 17 "BUT THE WORLD RECLAIMS EVERY SECOND YOU BORROW."))
      , modeLine 72 (Color brass (normalIcon 17)) "NORMAL" brass "WHEELS TURN AT 1X. THEY KILL ON TOUCH."
      , modeLine 22 (Color frost (frozenIcon 17)) "FROZEN" frost "HOLD SPACE. WHEELS STOP AND DEBT GROWS, 3 SECONDS AT MOST."
      , modeLine (-28) (Color ember (repayIcon 17)) "REPAYING" ember "LET GO. WHEELS RUN AT 2X UNTIL THE DEBT IS PAID. NO FREEZING."
      , Color brassDim (quad (-360) (-72) 360 (-70))
      , at 0 (-104) (Color cream (label 14 "WASD OR ARROWS MOVE     HOLD SPACE FREEZE"))
      , at 0 (-132) (Color cream (label 14 "P PAUSE     R RESTART     ESC QUIT"))
      , at 0 (-160) (Color (withA 0.8 verdigris) (label 14 "A STOPPED WHEEL STILL CUTS.  REACH THE EXIT."))
      ]
    modeLine y icon name c detail = Pictures
      [ at (-500) (y + 8) icon
      , at (-474) y (Color c (labelL 19 name))
      , at (-300) (y + 2) (Color cream (labelL 13.5 detail))
      ]

-- | Mode icons: shape alone tells the modes apart.
normalIcon, frozenIcon, repayIcon :: Float -> Picture
normalIcon s = Polygon [(-s * 0.6, -s * 0.8), (s * 0.8, 0), (-s * 0.6, s * 0.8)]
frozenIcon s = Pictures
  [ quad (-s * 0.75) (-s * 0.8) (-s * 0.2) (s * 0.8), quad (s * 0.2) (-s * 0.8) (s * 0.75) (s * 0.8) ]
repayIcon s = Pictures
  [ Polygon [(-s, -s * 0.8), (0, 0), (-s, s * 0.8)], Polygon [(0, -s * 0.8), (s, 0), (0, s * 0.8)] ]

-- | The window keeps the 16:9 picture: scale to fit, then cover the bars.
render :: Config -> Level -> Scene -> Game -> Fx -> (Int, Int) -> Picture
render cfg level scene game fx (winW, winH) =
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
  in Pictures [Scale k k (frame cfg level scene game fx), bars]

frame :: Config -> Level -> Scene -> Game -> Fx -> Picture
frame cfg level scene game fx
  | gPhase game == Title = Pictures [scBed scene, gearsLayer, overlay]
  | otherwise = Pictures
  [ scBed scene
  , gearsLayer
  , scDialAt
  , scFloors scene
  , Color (withA (0.34 * frostF) frostDeep) (scFloorShape scene)
  , Color (withA (0.13 * strainF) ember) (scFloorShape scene)
  , Pictures (scRails scene)
  , repayStreaks
  , exitPad
  , hints
  , trails
  , Pictures (zipWith hazardPic (lvlHazardsPositions) [0 :: Int ..])
  , playerPic
  , sparks
  , rings
  , Color (withA (0.9 * frostF) frost) (scFrostEdge scene)
  , strainEdges
  , hud
  , overlay
  ]
  where
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
    -- The machine stays quiet behind the play: tinted, but well below the wheels.
    machineTint = blend strainF (blend frostF (blend 0.5 plateHi brassDim) (blend 0.45 plateHi frostDeep))
                    (blend 0.42 plateHi emberDeep)

    gearsLayer = Color machineTint (Pictures
      [ at x y (Rotate (clock * rate) cog) | (x, y, rate, cog) <- scGears scene ])

    -- The dial's hand is the hazard clock itself: one turn per gallery cycle.
    scDialAt = at 213 30 (Pictures
      [ scDial scene
      , Color (blend strainF (blend frostF brass frost) ember)
          (Rotate (clock * 2) (thickSeg 5 (0, -22) (0, 118)))
      , Color cream (circleSolid 8)
      , Color (blend strainF (blend frostF brass frost) ember) (at 0 (-84) (label 24 rateText))
      , if mode == Frozen then Color frost (at 0 34 (frozenIcon 18)) else Blank
      ])
    rateText = case mode of
      Normal   -> "1X"
      Frozen   -> "0X"
      Repaying -> "2X"

    -- Streaks along the rails while repaying, moving with the wheel's direction.
    repayStreaks
      | strainF < 0.02 || reduced = Blank
      | otherwise =
          let offsetX = fromIntegral ((fxTick fx * 7) `mod` 60)
              c = withA (0.65 * strainF) ember
              gallery = at (if galleryEast then -40 + offsetX else 540 - offsetX) 260
                          (Rotate (if galleryEast then 0 else 180) (scChevrons scene))
          in Color c gallery
    galleryEast =
      let route = hazardRoute (lvlHazards level !! 1)
          Vec a _ = routePosition route (tsClock time)
          Vec b _ = routePosition route (tsClock time + 1)
      in b > a

    exitPad =
      let (l, b, r, t) = rectF (lvlExit level)
          cx = (l + r) / 2
          cy = (b + t) / 2
          pulse = if moving then 1 + 0.04 * sin (tick * 0.05) else 1
      in at cx cy (Pictures
           [ glow verdigris 38 0.55
           , Color (withA 0.25 verdigris) (circleSolid 34)
           , Scale pulse pulse (Rotate (tick * 0.3) (scExitMark scene))
           , at 0 (-7) (Color cream (label 15 "EXIT"))
           ])

    trails = Pictures
      [ Color (withA (0.34 * (1 - fromIntegral i / fromIntegral (cfgTrailLength cfg)) * max 0.2 strainF) ember)
          (at x y (circleSolid (realToFrac (cfgHazardRadius cfg) * (0.86 - 0.04 * fromIntegral i))))
      | samples <- fxTrails fx
      , (i, p) <- zip [0 :: Int ..] samples
      , i `mod` 2 == 1
      , let (x, y) = v2 p ]

    hazardPic pos i =
      let (x, y) = v2 pos
          route = hazardRoute (lvlHazards level !! i)
          period = fromIntegral (routePeriod route) :: Float
          spin = negate (clock * 720 / period) * (if i == 0 then 0.5 else 1)
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

    -- Context hints, in the panel above the rooms.
    hints =
      let (a, b) = hintLines level game
      in at (-576) 288 (Pictures
           [ Color modeColor (at 0 (-30) (labelL 12.5 a))
           , Color (withA 0.85 cream) (at 0 (-54) (labelL 12.5 b))
           , Color brassDim (at 0 (-8) (labelL 9.5 "FOREMAN'S NOTE"))
           ])
    modeColor = case mode of
      Normal   -> brass
      Frozen   -> frost
      Repaying -> ember

    hud = Pictures
      [ scModeHud scene
      , modePlate
      , meter
      , debtPlate
      ]

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
          nearCap = tsDebt time >= cfgMaxDebt cfg - 45 && mode == Frozen
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
           , if nearCap then at r (-303) (Color ember (labelR 9.5 "CAP NEAR: FORCED RELEASE")) else Blank
           ]

    debtPlate =
      let seconds = fromIntegral (tsDebt time) / fromIntegral (cfgTicksPerSecond cfg) :: Double
          ready = canFreeze time
          (status, statusC) = case mode of
            Normal   -> ("FREEZE READY", verdigris)
            Frozen   -> ("FREEZE HELD", frost)
            Repaying -> ("FREEZE LOCKED", ember)
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
      Title -> Pictures
        [ scTitle scene
        , at 0 (-228) (Color (withA (0.75 + 0.25 * sin (tick * 0.06)) brass) (label 24 "PRESS ENTER"))
        , if moving
            then at 0 40 (Color (withA 0.3 brass) (Rotate (tick * 2) (thickSeg 3 (0, 0) (0, 320))))
            else Blank
        ]
      Paused -> banner frost "PAUSED" "P TO RESUME    R TO RESTART"
        (if mode == Frozen && tsDebt time > 0
           then "A FREEZE DOES NOT SURVIVE A PAUSE: THE DEBT FALLS DUE ON RESUME."
           else "THE CLOCK AND THE DEBT ARE HOLDING.")
      Lost -> banner ember "CAUGHT" "ENTER OR R TO RETRY" (lossReason mode)
      Won -> banner verdigris "ESCAPED" "ENTER TO RUN IT AGAIN"
        (printf "OUT IN %.1f S, HAVING BORROWED %.1f S IN ALL."
           (secondsOf (gTicks game)) (secondsOf (gBorrowed game)))
      Playing -> Blank
    secondsOf n = fromIntegral n / fromIntegral (cfgTicksPerSecond cfg) :: Double

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
  Repaying -> "THE DEBT CAME DUE. LET GO ONLY WHERE THE WHEEL CANNOT REACH."

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

-- | Two short lines that fit where the player is and what the clock is doing.
hintLines :: Level -> Game -> (String, String)
hintLines level game
  | inStart && mode == Normal = ("TOO FAST TO DODGE.", "HOLD SPACE, THEN WALK AROUND IT.")
  | inSafe && mode == Repaying = ("THIS ROOM IS SAFE. WAIT OUT THE DEBT.", "NO FREEZE UNTIL THE METER EMPTIES.")
  | inSafe && mode == Frozen = ("THROUGH. LET GO OF SPACE.", "EVERY SECOND HELD MUST BE REPAID.")
  | inSafe = ("THE NORTH DOOR LEADS TO THE GALLERY.", "FOLLOW THE WHEEL. STOP IT AS IT TURNS.")
  | inRefuge && mode == Normal = ("WAIT FOR THE WHEEL TO PASS WEST,", "THEN RUN FOR THE EXIT DOOR.")
  | inRefuge && mode == Frozen = ("CLEAR OF THE WHEEL. LET GO.", "IT WILL PASS YOU AT DOUBLE SPEED.")
  | inRefuge = ("STAY PUT. THE WORLD IS CATCHING UP.", "NO FREEZE UNTIL THE METER EMPTIES.")
  | inExit = ("NO WHEEL REACHES THIS ROOM.", "THE WAY OUT IS AHEAD.")
  | mode == Frozen && inGallery = ("REACH THE REFUGE BEFORE YOU LET GO.", "THE GREEN ALCOVE, SOUTH WALL.")
  | mode == Frozen = ("A STOPPED WHEEL STILL CUTS.", "LET GO ONCE THROUGH THE EAST DOOR.")
  | mode == Repaying = ("THE WORLD TAKES BACK WHAT YOU BORROWED.", "NO FREEZE UNTIL IT IS PAID. GET CLEAR!")
  | inGallery = ("STOP THE WHEEL ONCE IT IS PAST THE REFUGE.", "IT CANNOT BE PASSED IN HERE.")
  | otherwise = ("HOLD SPACE TO STOP THE WHEEL.", "THEN WALK AROUND IT.")
  where
    mode = tsMode (gTime game)
    p = gPlayer game
    within (Rect l b r t) = let Vec x y = p in x >= l && x <= r && y >= b && y <= t
    inStart = within (lvlStartArea level)
    inSafe = within (lvlSafeArea level)
    inRefuge = within (lvlRefuge level)
    inExit = within (Rect 470 (-20) 600 214)
    inGallery = within (Rect (-70) 214 600 306)
