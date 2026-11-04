-- | Deco lettering, drawn from strokes. Gloss's own Text is a one-pixel font with no
-- width metric; this one centres exactly and holds its weight.
--
-- The proportions are the 1930s signwriter's: round letters are very wide, E F L S are
-- narrow, and crossbars sit high. Lowercase letters come out as small capitals.
module Font
  ( textL, textC, textR
  , textW
  , weighted
  , numeralsC, numeralsR
  ) where

import Data.Char (isLower, toUpper)
import Draw
import Graphics.Gloss

type Stroke = [(Float, Float)]

-- | An arc, cut every @step@ degrees. Text is a big part of each frame's shape count,
-- so small letters are cut as coarsely as still looks round.
curve :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Stroke
curve step cx cy rx ry a0 a1 =
  [ (cx + rx * cos t, cy + ry * sin t)
  | i <- [0 .. n]
  , let t = (a0 + (a1 - a0) * fromIntegral i / fromIntegral n) * pi / 180 ]
  where n = max 2 (ceiling (abs (a1 - a0) / step)) :: Int

-- | Strokes on a grid 6 tall. @step@ sets how finely curves are cut.
glyph :: Float -> Char -> [Stroke]
glyph step ch = case ch of
  'A' -> [[(0,0),(2.4,6),(4.8,0)], [(1.0,1.7),(3.8,1.7)]]
  'B' -> [ [(0,3.9),(0,6),(1.8,6)] ++ a 1.8 4.95 1.2 1.05 90 (-90) ++ [(0,3.9)]
         , [(0,3.9),(1.9,3.9)] ++ a 1.9 1.95 1.9 1.95 90 (-90) ++ [(0,0),(0,3.9)] ]
  'C' -> [a 3 3 3 3 45 315]
  'D' -> [[(0,0),(0,6),(1.5,6)] ++ a 1.5 3 3 3 90 (-90) ++ [(0,0)]]
  'E' -> [[(3,6),(0,6),(0,0),(3,0)], [(0,3.9),(2.3,3.9)]]
  'F' -> [[(3,6),(0,6),(0,0)], [(0,3.9),(2.3,3.9)]]
  'G' -> [a 3 3 3 3 45 320 ++ [(5.3,2.6),(3.3,2.6)]]
  'H' -> [[(0,0),(0,6)], [(4,0),(4,6)], [(0,3.9),(4,3.9)]]
  'I' -> [[(0,0),(0,6)]]
  'J' -> [[(2.6,6),(2.6,1.3)] ++ a 1.3 1.3 1.3 1.3 0 (-170)]
  'K' -> [[(0,0),(0,6)], [(3.6,6),(0,2.7)], [(1.3,3.9),(3.8,0)]]
  'L' -> [[(0,6),(0,0),(2.9,0)]]
  'M' -> [[(0,0),(0.7,6),(2.9,1.2),(5.1,6),(5.8,0)]]
  'N' -> [[(0,0),(0,6),(4.2,0),(4.2,6)]]
  'O' -> [a 3 3 3 3 90 450]
  'P' -> [[(0,0),(0,6),(1.9,6)] ++ a 1.9 4.6 1.5 1.4 90 (-90) ++ [(0,3.2)]]
  'Q' -> [a 3 3 3 3 90 450, [(3.6,1.4),(5.8,-0.5)]]
  'R' -> [[(0,0),(0,6),(1.9,6)] ++ a 1.9 4.6 1.5 1.4 90 (-90) ++ [(0,3.2)], [(1.6,3.2),(3.7,0)]]
  'S' -> [a 1.6 4.5 1.5 1.5 30 270 ++ a 1.6 1.5 1.7 1.5 90 (-150)]
  'T' -> [[(0,6),(4,6)], [(2,6),(2,0)]]
  'U' -> [[(0,6),(0,2)] ++ a 2.1 2 2.1 2 180 360 ++ [(4.2,6)]]
  'V' -> [[(0,6),(2.3,0),(4.6,6)]]
  'W' -> [[(0,6),(1.6,0),(3.1,5),(4.6,0),(6.2,6)]]
  'X' -> [[(0,0),(3.8,6)], [(0,6),(3.8,0)]]
  'Y' -> [[(0,6),(2,3),(4,6)], [(2,3),(2,0)]]
  'Z' -> [[(0,6),(3.8,6),(0,0),(3.8,0)]]
  '0' -> [a 2 3 2 3 90 450]
  '1' -> [[(0.9,4.7),(2.2,6),(2.2,0)]]
  '2' -> [a 2 4.2 1.9 1.8 165 (-35) ++ [(0,0),(4,0)]]
  '3' -> [a 1.9 4.6 1.8 1.4 150 (-90) ++ a 1.9 1.6 2.1 1.6 90 (-150)]
  '4' -> [[(3,0),(3,6),(0,1.9),(4,1.9)]]
  '5' -> [[(3.6,6),(0.5,6),(0.2,3.3)] ++ a 2 1.9 2 1.9 125 (-145)]
  '6' -> [a 2.1 3 2 3 60 180 ++ a 2 1.9 2 1.9 180 540]
  '7' -> [[(0,6),(4,6),(1.3,0)]]
  '8' -> [a 2 4.6 1.6 1.4 (-90) 270, a 2 1.6 2 1.6 90 450]
  '9' -> [a 1.9 3 2 3 240 360 ++ a 2 4.1 2 1.9 0 360]
  '.' -> [[(0.3,0),(0.3,0.5)]]
  ',' -> [[(0.5,0.5),(0,-0.9)]]
  ':' -> [[(0.3,0.5),(0.3,1)], [(0.3,3.6),(0.3,4.1)]]
  '\'' -> [[(0.3,6),(0.3,4.4)]]
  '!' -> [[(0.3,6),(0.3,1.9)], [(0.3,0),(0.3,0.5)]]
  '-' -> [[(0,2.8),(2.4,2.8)]]
  '/' -> [[(0,-0.4),(2.8,6.4)]]
  '*' -> [[(0,3),(0.9,3.9),(1.8,3),(0.9,2.1),(0,3)]]  -- a small lozenge, for separators
  _   -> []
  where a = curve step

-- | Everything on one grid, at one drawing size.
layout :: Float -> Float -> String -> ([Stroke], Float)
layout size track = foldl place ([], 0)
  where
    k = size / 6
    place (acc, x) c
      | c == ' ' = (acc, x + 2.8 * k + track)
      | otherwise =
          let scaleBy = if isLower c then 0.8 else 1
              strokes = glyph fineness (toUpper c)
              width = case concat strokes of
                [] -> 2
                ps -> maximum (map fst ps)
              m = k * scaleBy
              placed = [ [ (x + px * m, py * m) | (px, py) <- s ] | s <- strokes ]
          in (acc ++ placed, x + width * m + 1.5 * k + track)
    fineness | size >= 40 = 18
             | size >= 18 = 30
             | otherwise  = 45

defaultWeight :: Float -> Float
defaultWeight size = max 1.25 (size * 0.12)

tracking :: Float -> Float
tracking size = if size < 18 then 0.8 else 0.6

-- | Width in canvas units of a line, at a size and with the default tracking.
textW :: Float -> String -> Float
textW size str =
  let (_, w) = layout size (tracking size) str
  in max 0 (w - 1.5 * size / 6 - tracking size)

draw :: Float -> Float -> String -> Picture
draw size weight str = Pictures (concatMap stroke strokes)
  where
    (strokes, _) = layout size (tracking size) str
    -- The largest letters get round joints so curves stay clean. The rest keep cheap
    -- square ends, which are too small to see.
    stroke s
      | size >= 40 = zipWith (plainSeg weight) s (drop 1 s)
                       ++ [ at p (ngon 10 (weight / 2)) | p <- s ]
      | otherwise = case s of
          [p] -> [seg weight p p]
          _   -> zipWith (seg weight) s (drop 1 s)

-- | Left-aligned at the origin, baseline at y = 0. Colourless, to be tinted and baked.
textL, textC, textR :: Float -> String -> Picture
textL size = draw size (defaultWeight size)
textC size str = Translate (negate (textW size str) / 2) 0 (textL size str)
textR size str = Translate (negate (textW size str)) 0 (textL size str)

-- | As 'textL', with the pen weight chosen by hand.
weighted :: Float -> Float -> String -> Picture
weighted = draw

-- | Numerals in fixed cells, so that a running timer does not jitter sideways.
numerals :: Float -> Float -> String -> (Picture, Float)
numerals size weight str = (Pictures (go 0 str), total)
  where
    cell = size * 0.82
    dotCell = size * 0.34
    adv c = if c == '.' then dotCell else cell
    total = sum (map adv str)
    go _ [] = []
    go x (c : cs) =
      let inner = textW size [c]
          slack = (adv c - inner) / 2
      in Translate (x + slack) 0 (weighted size weight [c]) : go (x + adv c) cs

numeralsC, numeralsR :: Float -> Float -> String -> Picture
numeralsC size weight str =
  let (p, w) = numerals size weight str in Translate (negate w / 2) 0 p
numeralsR size weight str =
  let (p, w) = numerals size weight str in Translate (negate w) 0 p
