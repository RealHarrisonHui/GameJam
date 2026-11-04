-- | The Meridian Time Bank: everything in the level that never moves, built once.
module Scene
  ( Scene (..)
  , mkScene
  , dialACentre
  , dialBCentre
  , tubeY
  , exitLampY
  ) where

import Dial
import Draw
import Font
import Game.Level (centreline)
import Game.Types
import Graphics.Gloss

data Scene = Scene
  { scBoard :: Picture   -- ^ The whole level's stonework, floor and fixed brass.
  , scWheel :: Picture   -- ^ The great wheel in the mural. Turned by Render.
  , scVault :: Picture   -- ^ The exit's turning spokes.
  , scTitle :: Picture   -- ^ The title screen's fixed art.
  }

dialACentre, dialBCentre :: (Float, Float)
dialACentre = (730, 300)
dialBCentre = (360, 300)

tubeY :: Float
tubeY = 300

exitLampY :: Float
exitLampY = 518

mkScene :: Level -> Scene
mkScene level = Scene
  { scBoard = bake (board level)
  , scWheel = bake wheel
  , scVault = bake vaultSpokes
  , scTitle = bake titleArt
  }

-- The board -----------------------------------------------------------------------

board :: Level -> Picture
board level = Pictures
  [ walls level
  , veins
  , boxPanels
  , mural
  , frieze
  , hudBase
  , skirting
  , floors level
  , edges
  , doorHousing DoorA (lvDoorA level)
  , doorHousing DoorB (lvDoorB level)
  , exitStatic (f2 (lvExit level))
  , machine
  ]

walls :: Level -> Picture
walls level = Color marble (Pictures (map rectOf (lvWalls level)))

rectOf :: Rect -> Picture
rectOf (Rect l t r b) = wRect (d l) (d t) (d r) (d b)

d :: Double -> Float
d = realToFrac

-- | Polished stone: pale wandering veins, in a few families running the same way.
veins :: Picture
veins = Pictures
  [ Color (fade (0.14 + 0.28 * rnd i 0 2) vein) (Line (map wp (walkFrom i)))
  | i <- [0 .. 44 :: Int] ]
  where
    walkFrom i =
      let x0 = 1280 * rnd i 1 3
          y0 = 108 + 612 * rnd i 2 3
          heading = 0.6 + 0.5 * rnd i 3 3
          stepFrom (x, y, k) =
            let a = heading + 1.3 * (rnd i k 5 - 0.5)
                l = 18 + 34 * rnd i k 6
            in (x + l * cos a, y + l * sin a, k + 1)
          pts = take 9 (iterate stepFrom (x0, y0, 0 :: Int))
      in [ (x, y) | (x, y, _) <- pts ]

-- | The floor: travertine, with inlaid chevrons that lead the way.
floors :: Level -> Picture
floors level = Pictures
  [ Color floorC (Pictures (map rectOf (lvCorridors level)))
  , speckle
  , Color (fade 0.16 inlay) (Pictures (concatMap joints legs))
  , Pictures (concatMap borderLines legs)
  , Pictures (concatMap chevrons legs)
  , Pictures (map (medallion . f2) (take 4 (drop 1 centreline)))
  , startPlate (f2 (lvStart level))
  ]
  where
    legs = zip (map f2 centreline) (map f2 (drop 1 centreline))
    speckle = Pictures
      [ Color (if even i then fade 0.35 floorLo else fade 0.28 (hex 0xEFE5C8)) (wRect x y (x + 2) (y + 2))
      | (i, Rect l t r b) <- zip [0 :: Int ..] (concat (replicate 20 (lvCorridors level)))
      , let x = d l + d (r - l) * rnd i 4 7
            y = d t + d (b - t) * rnd i 5 7 ]
    joints ((x1, y1), (x2, y2))
      | y1 == y2 = [ Line (map wp [(x, y1 - 36), (x, y1 + 36)])
                   | x <- [min x1 x2, min x1 x2 + 72 .. max x1 x2] ]
      | otherwise = [ Line (map wp [(x1 - 36, y), (x1 + 36, y)])
                    | y <- [min y1 y2, min y1 y2 + 72 .. max y1 y2] ]
    borderLines ((x1, y1), (x2, y2))
      | y1 == y2 = [ Color (fade 0.55 inlay) (wRect (min x1 x2) (y1 + o) (max x1 x2) (y1 + o + 1.6))
                   | o <- [-29, 27.4] ]
      | otherwise = [ Color (fade 0.55 inlay) (wRect (x1 + o) (min y1 y2) (x1 + o + 1.6) (max y1 y2))
                    | o <- [-29, 27.4] ]
    chevrons (a@(x1, y1), (x2, y2)) =
      let len = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
          (ux, uy) = ((x2 - x1) / len, (y2 - y1) / len)
          angle = atan2 uy ux * 180 / pi
          marks = [46, 104 .. len - 52]
          isDoorNear (x, y) =
            any (\(Rect l t r b) -> x > d l - 34 && x < d r + 34 && y > d t && y < d b)
                [lvDoorA level, lvDoorB level]
      in [ wAt p (Rotate angle (Color (fade 0.7 inlay) chevron))
         | m <- marks
         , let p = (fst a + ux * m, snd a + uy * m)
         , not (isDoorNear p) ]
    chevron = Pictures [ plainSeg 3 (-6, 10) (5, 0), plainSeg 3 (5, 0) (-6, -10) ]

medallion :: (Float, Float) -> Picture
medallion p = wAt p $ Pictures
  [ Color (fade 0.5 inlay) (band 20 24 25.5)
  , Color (fade 0.4 inlay) (star 8 7 22)
  , Color (fade 0.8 inlay) (ngon 12 5)
  ]

startPlate :: (Float, Float) -> Picture
startPlate p = wAt p $ Pictures
  [ Color (fade 0.5 inlay) (band 24 26 28)
  , Color (fade 0.25 inlay) (ngon 36 26)
  , Translate 0 (-4) (Color (fade 0.85 brassLo) (textC 9 "START"))
  ]

-- | Brass trim and cast shadows where floor meets wall. Light falls from the north-west,
-- so the north and west edges of the floor sit in shade.
edges :: Picture
edges = Pictures
  [ Pictures [ shadeH x0 x1 y | (y, x0, x1) <- northEdges ]
  , Pictures [ shadeV y0 y1 x | (x, y0, y1) <- westEdges ]
  , Pictures [ trimH x0 x1 y (-1) | (y, x0, x1) <- northEdges ]
  , Pictures [ trimH x0 x1 y 1 | (y, x0, x1) <- southEdges ]
  , Pictures [ trimV y0 y1 x (-1) | (x, y0, y1) <- westEdges ]
  , Pictures [ trimV y0 y1 x 1 | (x, y0, y1) <- eastEdges ]
  ]
  where
    northEdges = [(144, 84, 876), (384, 204, 804), (564, 276, 1116)]
    southEdges = [(216, 84, 804), (456, 276, 876), (636, 204, 1116)]
    westEdges = [(84, 144, 216), (804, 216, 384), (204, 384, 636)]
    eastEdges = [(876, 144, 456), (276, 456, 564), (1116, 564, 636)]
    shadeH x0 x1 y = Pictures
      [ Color (fade 0.09 ink) (wRect x0 y x1 (y + h)) | h <- [4, 8, 13, 19] ]
    shadeV y0 y1 x = Pictures
      [ Color (fade 0.07 ink) (wRect x y0 (x + h) y1) | h <- [3, 7, 12] ]
    -- side: which way the wall lies from the edge
    trimH x0 x1 y side = Pictures
      [ Color brassLo (wRect (x0 - 4) (y + side * 5) (x1 + 4) y)
      , Color brass (wRect (x0 - 4) (y + side * 3.2) (x1 + 4) (y + side * 1))
      , Color (fade 0.8 brassHi) (wRect (x0 - 4) (y + side * 2.2) (x1 + 4) (y + side * 1))
      ]
    trimV y0 y1 x side = Pictures
      [ Color brassLo (wRect (x + side * 5) (y0 - 4) x (y1 + 4))
      , Color brass (wRect (x + side * 3.2) (y0 - 4) (x + side * 1) (y1 + 4))
      , Color (fade 0.8 brassHi) (wRect (x + side * 2.2) (y0 - 4) (x + side * 1) (y1 + 4))
      ]

-- | Door furniture that stays: threshold, jambs, the wall tag and the pipe to its dial.
doorHousing :: DoorId -> Rect -> Picture
doorHousing door (Rect l t r b) = Pictures
  [ Color (fade 0.8 brassLo) (wRect (x0 - 2) y0 x0 y1)
  , Color (fade 0.8 brassLo) (wRect x1 y0 (x1 + 2) y1)
  , jamb (y0 - 13) y0
  , jamb y1 (y1 + 13)
  , tag
  ]
  where
    x0 = d l
    x1 = d r
    y0 = d t
    y1 = d b
    cx = (x0 + x1) / 2
    jamb ya yb = Pictures
      [ Color brassLo (wRect (x0 - 8) ya (x1 + 8) yb)
      , Color brass (wRect (x0 - 6) (ya + 1.5) (x1 + 6) (yb - 1.5))
      , Color brassHi (wRect (x0 - 6) (ya + 1.5) (x1 + 6) (ya + 3))
      ]
    (tagY, pipeA, pipeB) = case door of
      DoorA -> (y0 - 28, (cx, y1 + 13), (cx, y1 + 40))    -- tag above, pipe below to the dial
      DoorB -> (y1 + 30, (cx, y0 - 13), (cx, y0 - 20))
    tag = Pictures
      [ Color brassLo (wSeg 7 pipeA pipeB)
      , Color brass (wSeg 4 pipeA pipeB)
      , wAt (cx, tagY) (emblem door 12)
      ]

-- | The clocks, the tube between them and their labels.
machine :: Picture
machine = Pictures
  [ Color (fade 0.5 ink) (wRect 296 222 794 378)
  , Color (fade 0.8 brassLo) (frame 1.2 296 222 794 378)
  , wAt dialBCentre (dialStatic DoorB)
  , wAt dialACentre (dialStatic DoorA)
  , tubeStatic (fst dialBCentre) (fst dialACentre) tubeY
  , wAt (545, 256) (Color (fade 0.92 brassHi) (textC 13 "PNEUMATIC EXCHANGE"))
  , wAt (659, 277) (Color (doorLight DoorA) (textR 9 "TO DOOR A"))
  , wAt (431, 332) (Color (doorLight DoorB) (textL 9 "TO DOOR B"))
  , wAt (545, 349) keycap
  ]
  where
    keycap = Pictures
      [ Color brassLo (rectC 152 24)
      , Color (hex 0x18211F) (rectC 148 20)
      , Translate 0 (-5) (Color ivory (textC 11 "SPACE TO SWAP"))
      ]

-- | Rows of safe-deposit boxes set into the stone.
boxPanels :: Picture
boxPanels = Pictures
  [ boxes 100 230 4 3 44 42
  , boxes 18 398 4 7 43 33
  , Color (fade 0.8 brassLo) (frame 1.5 94 224 282 376)
  , Color (fade 0.8 brassLo) (frame 1.5 12 392 196 628)
  ]

boxes :: Float -> Float -> Int -> Int -> Float -> Float -> Picture
boxes x0 y0 cols rows w h = Pictures
  [ box (x0 + fromIntegral c * w) (y0 + fromIntegral r * h) (c * 13 + r * 7)
  | c <- [0 .. cols - 1], r <- [0 .. rows - 1] ]
  where
    box x y salt = Pictures
      [ Color (hex 0x241E12) (wRect x y (x + w - 2) (y + h - 2))
      , Color (mix (0.3 * rnd salt 1 9) (hex 0x8E733A) (hex 0xB39453)) (wRect (x + 1.5) (y + 1.5) (x + w - 3.5) (y + h - 3.5))
      , Color (hex 0xE3D5A7) (wRect (x + w / 2 - 8) (y + 7) (x + w / 2 + 6) (y + 12))
      , Color (hex 0x241E12) (wRect (x + w / 2 - 2.5) (y + h - 14) (x + w / 2 + 0.5) (y + h - 7))
      ]

-- | The mural beside the first bend: a sunburst, with the bank's great wheel in front.
mural :: Picture
mural = Pictures
  [ Color (hex 0x0C1C20) (wRect l t r b)
  , rays
  , wAt (cx, cy) (Pictures [Color (fade 0.65 ink) (ngon 48 118), Color brassLo (band 48 112 116)])
  , Color brass (frame 2.5 l t r b)
  , Color brassLo (frame 1 (l + 7) (t + 7) (r - 7) (b - 7))
  , steppedCorner l t 1 1
  , steppedCorner r t (-1) 1
  , steppedCorner l b 1 (-1)
  , steppedCorner r b (-1) (-1)
  , Color (hex 0x0C1C20) (wRect (cx - 138) (b - 34) (cx + 138) (b - 8))
  , Color brass (frame 1.2 (cx - 138) (b - 34) (cx + 138) (b - 8))
  , wAt (cx, b - 15.5) (Color brassHi (textC 11 "MERIDIAN TIME BANK  *  1931"))
  ]
  where
    l = 898
    t = 164
    r = 1258
    b = 436
    cx = (l + r) / 2
    cy = 296
    ox = cx
    oy = b - 8
    rays = Pictures
      [ Color (if even i then fade 0.75 (hex 0x1E4347) else fade 0.6 (hex 0x142F33))
          (wPoly [(ox, oy), edgePoint (th i), edgePoint (th (i + 1))])
      | i <- [0 .. 23 :: Int] ]
    th i = pi * fromIntegral i / 24 :: Float
    edgePoint a =
      let ux = cos a
          uy = negate (sin a)
          reach = minimum $
            [ (r - 6 - ox) / ux | ux > 0.0001 ]
            ++ [ (l + 6 - ox) / ux | ux < -0.0001 ]
            ++ [ (t + 6 - oy) / uy | uy < -0.0001 ]
      in (ox + ux * reach, oy + uy * reach)

steppedCorner :: Float -> Float -> Float -> Float -> Picture
steppedCorner x y sx sy = Pictures
  [ Color brass (wRect (x + sx * k) (y + sy * k) (x + sx * (k + 22 - 7 * n)) (y + sy * (k + 2)))
  | (n, k) <- [(0, 10), (1, 16), (2, 22)] ]

-- | The great wheel, around the origin.
wheel :: Picture
wheel = Pictures
  [ Color (hex 0x9C7E38) (band 40 83 95)
  , Pictures
      [ Rotate (fromIntegral i * 15) (Color (hex 0x9C7E38) (Polygon [(94, -5), (104, -2.5), (104, 2.5), (94, 5)]))
      | i <- [0 .. 23 :: Int] ]
  , Pictures
      [ Rotate (fromIntegral i * 60) (Pictures
          [ Color (hex 0xB08F40) (rect 20 (-4) 66 8) ])
      | i <- [0 .. 5 :: Int] ]
  , Color brassLo (ngon 28 24)
  , Color brass (ngon 28 20)
  , Color brassLo (ngon 20 9)
  ]

-- | The motto, and the exit sign.
frieze :: Picture
frieze = Pictures
  [ Color (hex 0x0C1C20) (wRect 420 474 1010 546)
  , Color brass (frame 2 420 474 1010 546)
  , Color brassLo (frame 1 426 480 1004 540)
  , Pictures [ Color brass (Pictures [wRect (x - 1) 486 (x + 1) 534]) | x <- [440, 452, 978, 990] ]
  , wAt (715, 518) (Color brassHi (textC 16 "WHAT YOU LEAVE BEHIND, WE LEND AHEAD"))
  , Color (hex 0x0C1C20) (wRect 1030 472 1130 552)
  , Color brass (frame 2 1030 472 1130 552)
  , wAt (1080, exitLampY - 8) (Color ivory (textC 24 "EXIT"))
  , wAt (1080, 538) (Color gold (Pictures
      [ plainSeg 4 (-14, 6) (0, -7), plainSeg 4 (0, -7) (14, 6) ]))
  ]

-- | The band for the HUD, and the stepped cornice between it and the first corridor.
hudBase :: Picture
hudBase = Pictures
  [ Color ink (wRect 0 0 1280 106)
  , Color (fade 0.3 marbleHi) (wRect 0 106 1280 144)
  , Pictures [ Color (fade 0.35 brassLo) (wRect x 110 (x + 2) 140) | x <- [14, 62 .. 1270] ]
  , Color brass (wRect 0 104 1280 107)
  , Color brassHi (wRect 0 104 1280 105)
  , wAt (28, 56) (Color brassHi (weighted 34 3.4 "BORROWED SECONDS"))
  , wAt (30, 80) (Color (fade 0.85 ivory) (textL 11 "THE MERIDIAN TIME BANK  *  AFTER HOURS"))
  ]

-- | Fluted skirting, with the controls set into it.
skirting :: Picture
skirting = Pictures
  [ Color ink (wRect 0 648 1280 720)
  , Pictures [ Color (fade 0.5 marbleHi) (wRect x 650 (x + 3) 720) | x <- [6, 22 .. 1276] ]
  , Color brass (wRect 0 646 1280 649)
  , Color (hex 0x0B1719) (wRect 170 662 1110 708)
  , Color (fade 0.8 brassLo) (frame 1.2 170 662 1110 708)
  , legend 190 "WASD / ARROWS" "MOVE"
  , legend 462 "SPACE" "SWAP TIME"
  , legend 700 "P" "PAUSE"
  , legend 862 "R" "RESTART"
  ]
  where
    legend x key word =
      let w = textW 12 key + 16
      in Pictures
        [ Color brassLo (wRect x 673 (x + w) 697)
        , Color (hex 0x202B2C) (wRect (x + 1.5) 674.5 (x + w - 1.5) 695.5)
        , wAt (x + 8, 691) (Color ivory (textL 12 key))
        , wAt (x + w + 10, 691) (Color brassHi (textL 12 word))
        ]

exitStatic :: (Float, Float) -> Picture
exitStatic p = wAt p $ Pictures
  [ Color (fade 0.5 ink) (ngon 40 35)
  , Color brassLo (ngon 40 34.5)
  , Color brass (ngon 40 33)
  , Color brassLo (ngon 36 29.5)
  , Color (mix 0.6 (hex 0xFFEDB8) gold) (ngon 36 28)
  , Color (hex 0xFFEDB8) (ngon 28 15)
  ]

vaultSpokes :: Picture
vaultSpokes = Pictures
  [ Color brassLo (band 20 16 18)
  , Pictures
      [ Rotate (fromIntegral i * 60) (Color brassLo (rect 4 (-1.2) 13 2.4))
      | i <- [0 .. 5 :: Int] ]
  , Color brassLo (ngon 14 5)
  ]

-- Title ---------------------------------------------------------------------------

titleArt :: Picture
titleArt = Pictures
  [ Color marble (wRect 0 0 1280 720)
  , Color (fade 0.6 ink) (wRect 0 0 1280 720)
  , veins
  , sun
  , Color brass (frame 3 22 22 1258 698)
  , Color brassLo (frame 1 32 32 1248 688)
  , steppedCorner 22 22 1 1
  , steppedCorner 1258 22 (-1) 1
  , steppedCorner 22 698 1 (-1)
  , steppedCorner 1258 698 (-1) (-1)
  , wAt (640, 142) (Color brassLo (Translate 3 (-3) (weightedC 68 7.5 "BORROWED SECONDS")))
  , wAt (640, 142) (Color brassHi (weightedC 68 7.5 "BORROWED SECONDS"))
  , wAt (640, 174) (Color ivory (textC 15 "THE MERIDIAN TIME BANK  *  AFTER HOURS"))
  , wAt (430, 330) (dialStatic DoorB)
  , wAt (850, 330) (dialStatic DoorA)
  , tubeStatic 430 850 330
  , wAt (640, 468) (Color ivory (textC 18 "Two time locks stand between you and the street."))
  , wAt (640, 498) (Color ivory (textC 18 "A door stays open only while its clock has time."))
  , wAt (640, 528) (Color brassHi (textC 18 "SPACE swaps what the two clocks hold, from anywhere."))
  , keys
  ]
  where
    weightedC size w str = Translate (negate (textW size str) / 2) 0 (weighted size w str)
    ox = 640
    oy = 720
    sun = Pictures
      [ Color (if even i then fade 0.34 (hex 0x1E4347) else fade 0.05 (hex 0x1E4347))
          (wPoly [(ox, oy), far (th i), far (th (i + 1))])
      | i <- [0 .. 35 :: Int] ]
    th i = pi * fromIntegral i / 36 :: Float
    far a = (ox + 1100 * cos a, oy - 1100 * sin a)
    keys = Pictures
      [ key 318 "WASD / ARROWS" "MOVE"
      , key 590 "SPACE" "SWAP"
      , key 750 "P" "PAUSE"
      , key 868 "R" "RESTART"
      ]
    key x k word =
      let w = textW 12 k + 16
      in Pictures
        [ Color brassLo (wRect x 568 (x + w) 592)
        , Color (hex 0x202B2C) (wRect (x + 1.5) 569.5 (x + w - 1.5) 590.5)
        , wAt (x + 8, 586) (Color ivory (textL 12 k))
        , wAt (x + w + 10, 586) (Color brassHi (textL 12 word))
        ]
