-- | The visual vocabulary of Escapement: palette, engraved lettering, gears.
-- "Glow" throughout is layered translucent shapes, not a real bloom.
module Art
  ( -- * Palette
    ink, plate, plateHi, floorTone, floorLine, brass, brassDim, cream, ember, emberDeep
  , frost, frostDeep, verdigris, verdDeep, withA, blend
    -- * Lettering
  , label, labelL, labelR, labelWidth
    -- * Shapes
  , quad, box, boxLines, thickSeg, ringAt, tickRing, glow, at, gearPic, escapeWheelPic
  , chevron, diamond, hexOutline
  ) where

import Data.Char (toUpper)
import Graphics.Gloss

rgb :: Float -> Float -> Float -> Color
rgb r g b = makeColor (r / 255) (g / 255) (b / 255) 1

ink, plate, plateHi, floorTone, floorLine, brass, brassDim, cream, ember, emberDeep :: Color
frost, frostDeep, verdigris, verdDeep :: Color
ink       = rgb 9 13 22
plate     = rgb 18 25 40
plateHi   = rgb 28 38 58
floorTone = rgb 45 56 78
floorLine = rgb 60 73 99
brass     = rgb 222 178 92
brassDim  = rgb 120 99 62
cream     = rgb 247 238 214
ember     = rgb 255 96 58
emberDeep = rgb 168 44 28
frost     = rgb 165 232 248
frostDeep = rgb 58 124 158
verdigris = rgb 104 224 180
verdDeep  = rgb 34 92 82

withA :: Float -> Color -> Color
withA alpha c = let (r, g, b, _) = rgbaOfColor c in makeColor r g b alpha

blend :: Float -> Color -> Color -> Color
blend t a b =
  let (r1, g1, b1, a1) = rgbaOfColor a
      (r2, g2, b2, a2) = rgbaOfColor b
      mix x y = x + (y - x) * t
  in makeColor (mix r1 r2) (mix g1 g2) (mix b1 b2) (mix a1 a2)

at :: Float -> Float -> Picture -> Picture
at = Translate

-- Shapes -----------------------------------------------------------------------

quad :: Float -> Float -> Float -> Float -> Picture
quad l b r t = Polygon [(l, b), (r, b), (r, t), (l, t)]

box :: Float -> Float -> Float -> Float -> Picture
box = quad

-- | A rectangle outline, drawn as thin quads so it keeps a real width.
boxLines :: Float -> Float -> Float -> Float -> Float -> Picture
boxLines w l b r t = Pictures
  [ quad l b r (b + w), quad l (t - w) r t, quad l b (l + w) t, quad (r - w) b r t ]

-- | A straight stroke with square ends.
thickSeg :: Float -> (Float, Float) -> (Float, Float) -> Picture
thickSeg w (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      len = max 0.001 (sqrt (dx * dx + dy * dy))
      ux = dx / len * w / 2
      uy = dy / len * w / 2
  in Polygon
       [ (x1 - ux + uy, y1 - uy - ux), (x2 + ux + uy, y2 + uy - ux)
       , (x2 + ux - uy, y2 + uy + ux), (x1 - ux - uy, y1 - uy + ux) ]

-- | A straight stroke ending exactly at its points.
buttSeg :: Float -> (Float, Float) -> (Float, Float) -> Picture
buttSeg w (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      len = max 0.001 (sqrt (dx * dx + dy * dy))
      nx = negate dy / len * w / 2
      ny = dx / len * w / 2
  in Polygon [(x1 + nx, y1 + ny), (x2 + nx, y2 + ny), (x2 - nx, y2 - ny), (x1 - nx, y1 - ny)]

ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y r w = at x y (ThickCircle r w)

-- | Clock graduations: every @major@th tick is longer.
tickRing :: Int -> Int -> Float -> Float -> Float -> Float -> Picture
tickRing n major r short long w = Pictures
  [ thickSeg w (r * cos a, r * sin a) ((r - l) * cos a, (r - l) * sin a)
  | i <- [0 .. n - 1]
  , let a = 2 * pi * fromIntegral i / fromIntegral n
        l = if i `mod` major == 0 then long else short ]

-- | Layered translucent discs fading outward.
glow :: Color -> Float -> Float -> Picture
glow c r strength = Pictures
  [ Color (withA (strength * k) c) (circleSolid (r * s))
  | (s, k) <- [(1.75, 0.10), (1.45, 0.16), (1.2, 0.24)] ]

-- | A cog made of a rim, spokes and square-ish teeth. Colourless, so the caller's
-- 'Color' tints it, and built once so it only costs a 'Rotate' per frame.
gearPic :: Int -> Float -> Float -> Picture
gearPic teeth outer root = Pictures $
  [ at 0 0 (ThickCircle (root - rim / 2) rim)
  , ThickCircle (root * 0.22) (root * 0.1)
  ] ++
  [ Rotate (fromIntegral i * 360 / 5) (quad (-(root * 0.035)) (root * 0.2) (root * 0.035) (root - rim / 2))
  | i <- [0 .. 4 :: Int] ] ++
  [ Polygon
      [ pt (a - half) (root - 1), pt (a + half) (root - 1)
      , pt (a + half * 0.6) outer, pt (a - half * 0.6) outer ]
  | i <- [0 .. teeth - 1]
  , let a = 2 * pi * fromIntegral i / fromIntegral teeth ]
  where
    rim = root * 0.11
    half = pi / fromIntegral teeth * 0.55
    pt a r = (r * cos a, r * sin a)

-- | The hazard: a watchmaker's escape wheel with raked, saw-like teeth. Tooth tips
-- sit on @outer@, the wheel's collision radius, so what you see is what can hit you.
escapeWheelPic :: Color -> Color -> Color -> Int -> Float -> Picture
escapeWheelPic toothC bodyC detailC teeth outer = Pictures $
  [ Color toothC (Pictures
      [ Polygon [pt a root, pt (a + step * 0.62) root, pt (a + step * 1.05) outer]
      | i <- [0 .. teeth - 1]
      , let a = step * fromIntegral i ])
  , Color bodyC (circleSolid root)
  , Color detailC (ThickCircle (root * 0.74) (outer * 0.06))
  , Color detailC (Pictures
      [ Rotate (fromIntegral i * 90) (quad (-(outer * 0.05)) 0 (outer * 0.05) (root * 0.74)) | i <- [0 .. 3 :: Int] ])
  , Color toothC (circleSolid (outer * 0.17))
  , Color cream (circleSolid (outer * 0.07))
  ]
  where
    root = outer * 0.74
    step = 2 * pi / fromIntegral teeth
    pt a r = (r * cos a, r * sin a)

chevron :: Float -> Float -> Picture
chevron w size = Pictures
  [ thickSeg w (-size, size) (0, 0), thickSeg w (-size, -size) (0, 0) ]

diamond :: Float -> Picture
diamond r = Polygon [(r, 0), (0, r), (-r, 0), (0, -r)]

hexOutline :: Float -> Float -> Picture
hexOutline r w = Pictures
  [ thickSeg w (pt i) (pt (i + 1)) | i <- [0 .. 5 :: Int] ]
  where
    pt i = let a = pi / 3 * fromIntegral i + pi / 6 in (r * cos a, r * sin a)

-- Lettering --------------------------------------------------------------------

-- | Engraved strokes on a 4 by 6 grid. Gloss's own Text is a one-pixel GLUT font
-- with no width metric, so we draw our own: it centres exactly and holds weight.
glyph :: Char -> [[(Float, Float)]]
glyph ch = case ch of
  'A' -> [[(0,0),(0,4),(2,6),(4,4),(4,0)], [(0,2.2),(4,2.2)]]
  'B' -> [[(0,0),(0,6),(3,6),(4,5),(4,4),(3,3.1),(0,3.1)], [(3,3.1),(4,2.2),(4,1),(3,0),(0,0)]]
  'C' -> [[(4,5),(3,6),(1,6),(0,5),(0,1),(1,0),(3,0),(4,1)]]
  'D' -> [[(0,0),(0,6),(2.6,6),(4,4.6),(4,1.4),(2.6,0),(0,0)]]
  'E' -> [[(4,6),(0,6),(0,0),(4,0)], [(0,3.1),(3,3.1)]]
  'F' -> [[(4,6),(0,6),(0,0)], [(0,3.1),(3,3.1)]]
  'G' -> [[(4,5),(3,6),(1,6),(0,5),(0,1),(1,0),(3,0),(4,1),(4,2.8),(2.4,2.8)]]
  'H' -> [[(0,0),(0,6)], [(4,0),(4,6)], [(0,3.1),(4,3.1)]]
  'I' -> [[(1,6),(3,6)], [(2,6),(2,0)], [(1,0),(3,0)]]
  'J' -> [[(4,6),(4,1),(3,0),(1,0),(0,1)]]
  'K' -> [[(0,0),(0,6)], [(4,6),(0,2.4)], [(1.4,3.6),(4,0)]]
  'L' -> [[(0,6),(0,0),(4,0)]]
  'M' -> [[(0,0),(0,6),(2,3),(4,6),(4,0)]]
  'N' -> [[(0,0),(0,6),(4,0),(4,6)]]
  'O' -> [[(1,0),(0,1),(0,5),(1,6),(3,6),(4,5),(4,1),(3,0),(1,0)]]
  'P' -> [[(0,0),(0,6),(3,6),(4,5),(4,4),(3,3),(0,3)]]
  'Q' -> [[(1,0),(0,1),(0,5),(1,6),(3,6),(4,5),(4,1),(3,0),(1,0)], [(2.4,1.6),(4,0)]]
  'R' -> [[(0,0),(0,6),(3,6),(4,5),(4,4),(3,3),(0,3)], [(1.6,3),(4,0)]]
  'S' -> [[(4,5),(3,6),(1,6),(0,5),(0,4),(1,3.1),(3,3.1),(4,2.2),(4,1),(3,0),(1,0),(0,1)]]
  'T' -> [[(0,6),(4,6)], [(2,6),(2,0)]]
  'U' -> [[(0,6),(0,1),(1,0),(3,0),(4,1),(4,6)]]
  'V' -> [[(0,6),(2,0),(4,6)]]
  'W' -> [[(0,6),(1,0),(2,3.4),(3,0),(4,6)]]
  'X' -> [[(0,0),(4,6)], [(0,6),(4,0)]]
  'Y' -> [[(0,6),(2,3),(4,6)], [(2,3),(2,0)]]
  'Z' -> [[(0,6),(4,6),(0,0),(4,0)]]
  '0' -> [[(1,0),(0,1),(0,5),(1,6),(3,6),(4,5),(4,1),(3,0),(1,0)], [(0.8,1.2),(3.2,4.8)]]
  '1' -> [[(1,4.8),(2,6),(2,0)], [(1,0),(3,0)]]
  '2' -> [[(0,5),(1,6),(3,6),(4,5),(4,4),(0,0),(4,0)]]
  '3' -> [[(0,5),(1,6),(3,6),(4,5),(4,4),(3,3.1),(1.4,3.1)], [(3,3.1),(4,2.2),(4,1),(3,0),(1,0),(0,1)]]
  '4' -> [[(3,0),(3,6),(0,2),(4,2)]]
  '5' -> [[(4,6),(0,6),(0,3.4),(3,3.4),(4,2.5),(4,1),(3,0),(1,0),(0,1)]]
  '6' -> [[(4,5),(3,6),(1,6),(0,5),(0,1),(1,0),(3,0),(4,1),(4,2.2),(3,3.2),(0,3.2)]]
  '7' -> [[(0,6),(4,6),(1.4,0)]]
  '8' -> [[(1,3.1),(0,4),(0,5),(1,6),(3,6),(4,5),(4,4),(3,3.1),(1,3.1),(0,2.2),(0,1),(1,0),(3,0),(4,1),(4,2.2),(3,3.1)]]
  '9' -> [[(0,1),(1,0),(3,0),(4,1),(4,5),(3,6),(1,6),(0,5),(0,3.8),(1,2.8),(4,2.8)]]
  '.' -> [[(1.7,0),(2.3,0)]]
  '\'' -> [[(2.2,6),(1.8,4.4)]]
  ',' -> [[(2.2,0.4),(1.6,-0.9)]]
  ':' -> [[(1.7,0.6),(2.3,0.6)], [(1.7,4),(2.3,4)]]
  '!' -> [[(2,6),(2,1.8)], [(1.7,0),(2.3,0)]]
  '-' -> [[(0.6,3),(3.4,3)]]
  '/' -> [[(0.4,0),(3.6,6)]]
  '+' -> [[(2,1.4),(2,4.6)], [(0.4,3),(3.6,3)]]
  '>' -> [[(0.8,5),(3.4,3),(0.8,1)]]
  _   -> []

-- | Squared-off forms for small text. Gloss issues one draw per polygon, so the
-- chamfered letters above cost nearly twice as much, and at small sizes the
-- chamfers are barely visible anyway.
blocky :: Char -> [[(Float, Float)]]
blocky ch = case ch of
  'B' -> [[(4,3.1),(4,0),(0,0),(0,6),(3.4,6),(3.4,3.1),(0,3.1)]]
  'C' -> [[(4,6),(0,6),(0,0),(4,0)]]
  'D' -> [[(0,0),(0,6),(4,5),(4,1),(0,0)]]
  'G' -> [[(4,6),(0,6),(0,0),(4,0),(4,2.8),(2.4,2.8)]]
  'J' -> [[(4,6),(4,0),(0,0),(0,1.6)]]
  'O' -> [[(0,0),(0,6),(4,6),(4,0),(0,0)]]
  'P' -> [[(0,0),(0,6),(4,6),(4,3),(0,3)]]
  'Q' -> [[(0,0),(0,6),(4,6),(4,0),(0,0)], [(2.4,1.6),(4,0)]]
  'R' -> [[(0,0),(0,6),(4,6),(4,3),(0,3)], [(1.6,3),(4,0)]]
  'S' -> [[(4,6),(0,6),(0,3.1),(4,3.1),(4,0),(0,0)]]
  'U' -> [[(0,6),(0,0),(4,0),(4,6)]]
  '0' -> [[(0,0),(0,6),(4,6),(4,0),(0,0)], [(0.8,1.2),(3.2,4.8)]]
  '2' -> [[(0,6),(4,6),(4,3.1),(0,3.1),(0,0),(4,0)]]
  '3' -> [[(0,6),(4,6),(4,0),(0,0)], [(1.2,3.1),(4,3.1)]]
  '5' -> [[(4,6),(0,6),(0,3.1),(4,3.1),(4,0),(0,0)]]
  '6' -> [[(4,6),(0,6),(0,0),(4,0),(4,3.1),(0,3.1)]]
  '8' -> [[(0,0),(0,6),(4,6),(4,0),(0,0)], [(0,3.1),(4,3.1)]]
  '9' -> [[(0,0),(4,0),(4,6),(0,6),(0,3.1),(4,3.1)]]
  _   -> glyph ch

advance :: Float
advance = 5.5

-- | Width of a line of text at a cap height.
labelWidth :: Float -> String -> Float
labelWidth size str = (fromIntegral (length str) * advance - (advance - 4)) * size / 6

-- | Left-aligned at the origin. Colourless, so it can be tinted and cached.
lineL :: Float -> Float -> String -> Picture
lineL size weight str = Pictures (concatMap strokeOf placed)
  where
    k = size / 6
    placed = [ map (\(x, y) -> (px + x * k, y * k)) stroke
             | (i, c) <- zip [0 :: Int ..] (map toUpper str)
             , let px = fromIntegral i * advance * k
             , stroke <- (if size < 20 then blocky else glyph) c ]
    strokeOf points = zipWith segment points (drop 1 points) ++ joints points
    -- The biggest titles get butt-ended strokes with round joints; everything
    -- else stays cheap and its square ends are too small to see.
    large = size >= 60
    segment | large     = buttSeg weight
            | otherwise = thickSeg weight
    joints points
      | large     = [ at x y (circleSolid (weight / 2)) | (x, y) <- points ]
      | otherwise = []

weightFor :: Float -> Float
weightFor size = max 1.3 (size * 0.14)

labelL, labelR, label :: Float -> String -> Picture
labelL size = lineL size (weightFor size)
labelR size str = at (negate (labelWidth size str)) 0 (labelL size str)
label size str = at (negate (labelWidth size str) / 2) 0 (labelL size str)
