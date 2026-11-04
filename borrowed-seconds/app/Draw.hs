-- | Small drawing vocabulary shared by all the art: palette, shapes, noise, easing,
-- and the conversion from world coordinates (y down, as in the level plan) to Gloss.
module Draw
  ( -- * World coordinates
    wp, wAt, wRect, wSeg, wPoly, f2
    -- * Palette
  , ink, marble, marbleHi, vein, floorC, floorLo, inlay
  , brass, brassHi, brassLo, ivory, iron, ironHi, ember, gold
  , doorColor, doorLight
    -- * Colour
  , hex, fade, mix
    -- * Shapes
  , at, rect, rectC, ngon, ellipse, band, arcBand, seg, plainSeg, segs, diamond, star
  , glow, frame
    -- * Baking
  , bake
    -- * Noise and easing
  , rnd, clamp01, smooth, lerp, easeOutBack
  ) where

import Data.Bits (shiftR, xor)
import Game.Types (DoorId (..), Vec)
import Graphics.Gloss

-- World ---------------------------------------------------------------------------

f2 :: Vec -> (Float, Float)
f2 (x, y) = (realToFrac x, realToFrac y)

-- | World point to canvas point. The canvas is 1280 x 720, centred on the origin.
wp :: (Float, Float) -> (Float, Float)
wp (x, y) = (x - 640, 360 - y)

-- | Place a picture, drawn in y-up local units, at a world point.
wAt :: (Float, Float) -> Picture -> Picture
wAt p = at (wp p)

wRect :: Float -> Float -> Float -> Float -> Picture
wRect l t r b = Polygon (map wp [(l, t), (r, t), (r, b), (l, b)])

wSeg :: Float -> (Float, Float) -> (Float, Float) -> Picture
wSeg w a b = seg w (wp a) (wp b)

wPoly :: [(Float, Float)] -> Picture
wPoly = Polygon . map wp

-- Palette: malachite-black marble, travertine, brass, ivory, and one colour per door.

ink, marble, marbleHi, vein, floorC, floorLo, inlay :: Color
ink = hex 0x081214
marble = hex 0x10252A
marbleHi = hex 0x1A3A3F
vein = hex 0x2F6260
floorC = hex 0xD3C5A0
floorLo = hex 0xBDAD85
inlay = hex 0x9C8551

brass, brassHi, brassLo, ivory, iron, ironHi, ember, gold :: Color
brass = hex 0xC8A24A
brassHi = hex 0xF2D98F
brassLo = hex 0x6F5524
ivory = hex 0xF6EDD6
iron = hex 0x262B2E
ironHi = hex 0x4B5356
ember = hex 0x4A1A10
gold = hex 0xFFC94D

doorColor, doorLight :: DoorId -> Color
doorColor DoorA = hex 0xE0512B   -- vermilion
doorColor DoorB = hex 0x17AFA2   -- peacock
doorLight DoorA = hex 0xFF9A70
doorLight DoorB = hex 0x86F0E2

hex :: Int -> Color
hex v = makeColor (chan 16) (chan 8) (chan 0) 1
  where chan s = fromIntegral ((v `div` (2 ^ (s :: Int))) `mod` 256) / 255

-- | The same colour at this much of its own opacity.
fade :: Float -> Color -> Color
fade k c = let (r, g, b, a) = rgbaOfColor c in makeColor r g b (a * k)

-- | From the first colour (k = 0) to the second (k = 1).
mix :: Float -> Color -> Color -> Color
mix k a b =
  let (r1, g1, b1, a1) = rgbaOfColor a
      (r2, g2, b2, a2) = rgbaOfColor b
      f x y = x + (y - x) * k
  in makeColor (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

-- Shapes --------------------------------------------------------------------------

at :: (Float, Float) -> Picture -> Picture
at (x, y) = Translate x y

rect :: Float -> Float -> Float -> Float -> Picture
rect x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

rectC :: Float -> Float -> Picture
rectC w h = rect (-w / 2) (-h / 2) w h

ngon :: Int -> Float -> Picture
ngon n r = ellipse n r r

ellipse :: Int -> Float -> Float -> Picture
ellipse n rx ry = Polygon
  [ (rx * cos a, ry * sin a)
  | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]

-- | A full ring between two radii.
band :: Int -> Float -> Float -> Picture
band n r0 r1 = arcBand n r0 r1 0 360

-- | Part of a ring, angles in degrees anticlockwise from east. Built from plain quads
-- so that it can be baked, and never shows Gloss's own thick-arc faceting.
arcBand :: Int -> Float -> Float -> Float -> Float -> Picture
arcBand n r0 r1 a0 a1 = Pictures
  [ Polygon [p r0 s, p r1 s, p r1 t, p r0 t]
  | i <- [0 .. n - 1]
  , let s = a0 + (a1 - a0) * fromIntegral i / fromIntegral n
        t = a0 + (a1 - a0) * fromIntegral (i + 1) / fromIntegral n ]
  where p r d = let a = d * pi / 180 in (r * cos a, r * sin a)

-- | A stroke of width @w@, square-ended and slightly overlong so corners close.
seg :: Float -> (Float, Float) -> (Float, Float) -> Picture
seg w (x1, y1) (x2, y2)
  | len < 0.0001 = at (x1, y1) (rectC w w)
  | otherwise = Polygon
      [ (ax + nx, ay + ny), (bx + nx, by + ny), (bx - nx, by - ny), (ax - nx, ay - ny) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    ux = dx / len * w / 2
    uy = dy / len * w / 2
    nx = -uy
    ny = ux
    ax = x1 - ux
    ay = y1 - uy
    bx = x2 + ux
    by = y2 + uy

-- | A stroke that stops exactly at its ends.
plainSeg :: Float -> (Float, Float) -> (Float, Float) -> Picture
plainSeg w (x1, y1) (x2, y2)
  | len < 0.0001 = Blank
  | otherwise = Polygon [(x1 + nx, y1 + ny), (x2 + nx, y2 + ny), (x2 - nx, y2 - ny), (x1 - nx, y1 - ny)]
  where
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    nx = -dy / len * w / 2
    ny = dx / len * w / 2

segs :: Float -> [(Float, Float)] -> Picture
segs w ps = Pictures (zipWith (seg w) ps (drop 1 ps))

diamond :: Float -> Picture
diamond r = Polygon [(r, 0), (0, r), (-r, 0), (0, -r)]

-- | A many-pointed star: alternating long and short points, as thin triangles.
star :: Int -> Float -> Float -> Picture
star n r0 r1 = Pictures
  [ Polygon [ (r0 * cos (a - d), r0 * sin (a - d)), (r1 * cos a, r1 * sin a), (r0 * cos (a + d), r0 * sin (a + d)) ]
  | i <- [0 .. n - 1]
  , let a = 2 * pi * fromIntegral i / fromIntegral n
        d = pi / fromIntegral n ]

-- | Layered translucent discs: an artistic stand-in for a glow, not real bloom.
glow :: Color -> Float -> Int -> Picture
glow c radius layers = Pictures
  [ Color (fade (0.55 / fromIntegral layers) c) (ngon 28 (radius * fromIntegral i / fromIntegral layers))
  | i <- [layers, layers - 1 .. 1] ]

-- | An open rectangle in world coordinates: four strokes of width @w@ inside its edges.
frame :: Float -> Float -> Float -> Float -> Float -> Picture
frame w l t r b = Pictures
  [ wRect l t r (t + w), wRect l (b - w) r b, wRect l t (l + w) b, wRect (r - w) t r b ]

-- Baking --------------------------------------------------------------------------

-- | Flatten a static picture: transforms are multiplied into its coordinates and runs
-- of one colour share a single Color node. It draws exactly the same image, but Gloss
-- spends a matrix push on every transform and a state query on every colour, and those
-- dominate the frame for art made of thousands of small shapes.
--
-- Only handles what this game draws: polygons, lines and the structural nodes.
bake :: Picture -> Picture
bake = Pictures . regroup . flatten (1, 0, 0, 1, 0, 0) Nothing
  where
    flatten m c pic = case pic of
      Blank -> []
      Pictures ps -> concatMap (flatten m c) ps
      Color c' p -> flatten m (Just c') p
      Translate x y p -> flatten (mul m (1, 0, 0, 1, x, y)) c p
      Rotate deg p ->
        let t = deg * pi / 180 -- Gloss rotates clockwise
        in flatten (mul m (cos t, sin t, negate (sin t), cos t, 0, 0)) c p
      Scale sx sy p -> flatten (mul m (sx, 0, 0, sy, 0, 0)) c p
      Polygon ps -> [(c, Polygon (map (apply m) ps))]
      Line ps -> [(c, Line (map (apply m) ps))]
      other -> [(c, other)]
    mul (a, b, c, d, e, f) (a', b', c', d', e', f') =
      ( a * a' + b * c', a * b' + b * d'
      , c * a' + d * c', c * b' + d * d'
      , a * e' + b * f' + e, c * e' + d * f' + f )
    apply (a, b, c, d, e, f) (x, y) = (a * x + b * y + e, c * x + d * y + f)
    regroup [] = []
    regroup ((c, p) : rest) =
      let (same, others) = span ((== c) . fst) rest
          body = Pictures (p : map snd same)
      in maybe body (`Color` body) c : regroup others

-- Noise and easing ----------------------------------------------------------------

-- | Repeatable noise in [0,1) from a position and a salt.
rnd :: Int -> Int -> Int -> Float
rnd x y salt = fromIntegral (scramble (scramble (scramble (x * 374761 + 97) + y * 668265) + salt * 2654435) `mod` 100000) / 100000
  where
    scramble v =
      let a = (v * 1597334677) `mod` 4294967296
          b = a `xor` (a `shiftR` 15)
          c = (b * 2246822519) `mod` 4294967296
      in c `xor` (c `shiftR` 13)

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

smooth :: Float -> Float
smooth t = let u = clamp01 t in u * u * (3 - 2 * u)

lerp :: Float -> Float -> Float -> Float
lerp k a b = a + (b - a) * k

-- | Overshoots slightly, then settles: for panels arriving.
easeOutBack :: Float -> Float
easeOutBack t =
  let u = clamp01 t - 1
      s = 1.4
  in 1 + u * u * ((s + 1) * u + s)
