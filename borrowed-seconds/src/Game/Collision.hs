-- | Circle against rectangle. That is all the geometry this game needs.
module Game.Collision
  ( overlaps
  , pushOut
  , moveCircle
  , maxSubstep
  ) where

import Game.Types

-- | Below this, a touch is not an overlap. Pushing a circle out leaves it at the
-- radius give or take rounding, and that must not read as being inside.
slop :: Double
slop = 1e-7

closestPoint :: Rect -> Vec -> Vec
closestPoint (Rect l t r b) (x, y) = (max l (min r x), max t (min b y))

-- | Does a circle properly intersect a rectangle? Touching does not count.
overlaps :: Double -> Rect -> Vec -> Bool
overlaps radius rect p@(x, y) =
  let (cx, cy) = closestPoint rect p
      dx = x - cx
      dy = y - cy
  in dx * dx + dy * dy < (radius - slop) * (radius - slop)

-- | Move a circle the shortest way out of a rectangle, leaving it touching.
-- A centre inside the rectangle cannot be solved this way and is returned as it is,
-- for the caller to reject. Steps are small enough that this does not arise.
pushOut :: Double -> Rect -> Vec -> Vec
pushOut radius rect p@(x, y)
  | not (overlaps radius rect p) = p
  | d < slop = p
  | otherwise = (cx + dx / d * radius, cy + dy / d * radius)
  where
    (cx, cy) = closestPoint rect p
    dx = x - cx
    dy = y - cy
    d = sqrt (dx * dx + dy * dy)

-- | The longest single move collision will check. Thinner than anything solid, once
-- the circle's own width is counted, so nothing can be jumped over at any speed.
maxSubstep :: Double
maxSubstep = 2

-- | Walk a circle along a displacement in small steps, sliding along solids.
-- The solids are asked for afresh at each step, because a door the player has just
-- walked out of stops being passable at that very step.
moveCircle :: Double -> (Vec -> [Rect]) -> Vec -> Vec -> Vec
moveCircle radius solidsAt start (mx, my) = go steps start
  where
    dist = sqrt (mx * mx + my * my)
    steps = max 1 (ceiling (dist / maxSubstep)) :: Int
    sx = mx / fromIntegral steps
    sy = my / fromIntegral steps
    go 0 p = p
    go n p@(x, y) =
      let solids = solidsAt p
          settle q = foldl (flip (pushOut radius)) q solids
          moved = settle (settle (x + sx, y + sy))
      in if any (\r -> overlaps radius r moved) solids
           then p       -- wedged: stay put rather than end up inside something
           else go (n - 1 :: Int) moved
