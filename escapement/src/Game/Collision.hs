-- | Geometry: circles against walls, and swept circle against circle.
module Game.Collision
  ( rectContains
  , circleHitsRect
  , moveWithWalls
  , sweptContact
  ) where

import Game.Types

rectContains :: Rect -> Vec -> Bool
rectContains (Rect l b r t) (Vec x y) = x >= l && x <= r && y >= b && y <= t

-- | Strict overlap, so a circle resting exactly against a wall is not inside it.
circleHitsRect :: Double -> Vec -> Rect -> Bool
circleHitsRect radius (Vec x y) (Rect l b r t) =
  let dx = x - max l (min x r)
      dy = y - max b (min y t)
  in dx * dx + dy * dy < radius * radius

-- | Resolve each axis separately so the player slides along walls. Moves are
-- far shorter than the player's radius, so a wall cannot be jumped.
moveWithWalls :: [Rect] -> Double -> Vec -> Vec -> Vec
moveWithWalls walls radius start (Vec dx dy) =
  let free p = not (any (circleHitsRect radius p) walls)
      slide p step = head ([q | f <- [1, 0.75, 0.5, 0.25], let q = vAdd p (vScale f step), free q] ++ [p])
  in slide (slide start (Vec dx 0)) (Vec 0 dy)

-- | Do two circles, summed radius @r@, touch at any instant while moving
-- linearly from (p0,h0) to (p1,h1)? Working in the hazard's frame turns this
-- into one distance test against a single segment, so no speed can skip it.
sweptContact :: Double -> Vec -> Vec -> Vec -> Vec -> Bool
sweptContact r p0 p1 h0 h1 =
  let a = vSub p0 h0
      b = vSub p1 h1
      Vec ax ay = a
      Vec dx dy = vSub b a
      lenSq = dx * dx + dy * dy
      u | lenSq <= 0 = 0
        | otherwise  = max 0 (min 1 (negate (ax * dx + ay * dy) / lenSq))
  in vLen (vLerp u a b) <= r
