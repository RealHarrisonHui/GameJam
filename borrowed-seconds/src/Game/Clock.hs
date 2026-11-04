-- | Real time into whole ticks. The rules never see a clock: Main feeds elapsed
-- seconds through here and runs however many ticks come out.
module Game.Clock
  ( Accumulator
  , emptyAccumulator
  , maxCatchUpTicks
  , accumulate
  ) where

import Game.Types (ticksPerSecond)

-- | Unspent real time, in seconds.
newtype Accumulator = Accumulator Double
  deriving (Eq, Show)

emptyAccumulator :: Accumulator
emptyAccumulator = Accumulator 0

-- | The most ticks one frame may run. After a longer stall the excess is dropped,
-- so the doors never fast-forward through time the player did not see.
maxCatchUpTicks :: Int
maxCatchUpTicks = 6

-- | Add elapsed seconds, returning how many ticks to run now.
accumulate :: Double -> Accumulator -> (Int, Accumulator)
accumulate elapsed (Accumulator banked)
  | wanted > maxCatchUpTicks = (maxCatchUpTicks, Accumulator 0)
  | otherwise = (wanted, Accumulator (total - fromIntegral wanted * dt))
  where
    dt = 1 / fromIntegral ticksPerSecond
    total = banked + max 0 elapsed
    wanted = floor (total / dt)
