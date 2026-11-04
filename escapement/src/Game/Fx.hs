-- | Presentation state: eased values and short-lived decoration. It only reads
-- the game, so nothing here can affect gameplay, and any variation comes from
-- tick counters rather than a random source.
module Game.Fx
  ( Fx (..)
  , Particle (..)
  , Ring (..)
  , RingKind (..)
  , initialFx
  , stepFx
  ) where

import Game.Level (hazardPositions)
import Game.Types

data Particle = Particle
  { ptPos  :: !Vec
  , ptVel  :: !Vec
  , ptAge  :: !Int
  , ptLife :: !Int
  , ptCool :: !Bool  -- ^ verdigris rather than ember
  } deriving (Eq, Show)

data RingKind = RingFreeze | RingRelease | RingImpact | RingWin deriving (Eq, Show)

data Ring = Ring
  { rgPos  :: !Vec
  , rgAge  :: !Int
  , rgLife :: !Int
  , rgKind :: !RingKind
  } deriving (Eq, Show)

data Fx = Fx
  { fxTick      :: !Int     -- ^ runs on every screen; drives title and overlay motion
  , fxFrost     :: !Double  -- ^ eased 0..1 toward Frozen
  , fxStrain    :: !Double  -- ^ eased 0..1 toward Repaying
  , fxMeter     :: !Double  -- ^ eased debt as a fraction of the cap
  , fxHeading   :: !Double  -- ^ player facing, radians
  , fxMotion    :: !Double  -- ^ eased 0..1 movement, for squash and stretch
  , fxScreenAge :: !Int     -- ^ ticks on the current screen
  , fxTrails    :: [[Vec]]  -- ^ per hazard, newest first, bounded
  , fxParticles :: [Particle]
  , fxRings     :: [Ring]
  } deriving (Eq, Show)

initialFx :: Fx
initialFx = Fx 0 0 0 0 0 0 0 [] [] []

-- | Advance one simulation tick, given the game before and after it.
stepFx :: Config -> Level -> Game -> Game -> Fx -> Fx
stepFx cfg level before after fx
  | gPhase after == Playing && gTicks after == 0 && gTicks before /= 0 || screenStart =
      initialFx { fxTick = fxTick fx + 1 }
  | otherwise = fx
      { fxTick      = tick
      , fxFrost     = ease (if mode == Frozen then 1 else 0) (fxFrost fx)
      , fxStrain    = ease (if mode == Repaying then 1 else 0) (fxStrain fx)
      , fxMeter     = ease (fromIntegral debt / fromIntegral (cfgMaxDebt cfg)) (fxMeter fx)
      , fxHeading   = heading
      , fxMotion    = ease (min 1 (moved / cfgPlayerSpeed cfg)) (fxMotion fx)
      , fxScreenAge = if gPhase before == gPhase after then fxScreenAge fx + 1 else 0
      , fxTrails    = if reduced then [] else trails
      , fxParticles = if reduced then [] else take (cfgMaxParticles cfg) (newSparks ++ map ageSpark liveSparks)
      , fxRings     = take (cfgMaxRings cfg) (newRings ++ map ageRing liveRings)
      }
  where
    reduced = cfgReducedEffects cfg
    screenStart = gPhase after == Playing && gPhase before `elem` [Title, Won, Lost]
    tick = fxTick fx + 1
    ease target current = current + (target - current) * cfgEaseRate cfg
    time0 = gTime before
    time1 = gTime after
    mode = tsMode time1
    debt = tsDebt time1
    playing = gPhase after == Playing
    player = gPlayer after
    Vec dx dy = vSub player (gPlayer before)
    moved = vLen (Vec dx dy)
    heading | moved > 0.01 = atan2 dy dx
            | otherwise    = fxHeading fx

    -- Bounded trails: grow while repaying, shrink away otherwise.
    trails =
      let positions = hazardPositions level (tsClock time1)
          existing = fxTrails fx ++ repeat []
          update p old
            | mode == Repaying && playing = take (cfgTrailLength cfg) (p : old)
            | otherwise = take (max 0 (length old - 1)) old
      in zipWith update positions existing

    began  = tsMode time0 /= Frozen && mode == Frozen
    ended  = tsMode time0 == Frozen && mode /= Frozen
    lost   = gPhase before /= Lost && gPhase after == Lost
    won    = gPhase before /= Won && gPhase after == Won

    newRings = concat
      [ [Ring player 0 28 RingFreeze  | began]
      , [Ring player 0 22 RingRelease | ended]
      , [Ring player 0 30 RingImpact  | lost]
      , [Ring player 0 60 RingWin     | won]
      ]
    ageRing r = r { rgAge = rgAge r + 1 }
    liveRings = filter (\r -> rgAge r < rgLife r) (fxRings fx)

    -- A fixed hash of the tick stands in for randomness.
    wobble :: Int -> Double
    wobble n = fromIntegral ((n * 2654435761 + 40503) `mod` 1000) / 1000

    burst count speed life cool =
      [ Particle player (vScale (speed * (0.5 + 0.5 * wobble (tick + i))) (Vec (cos a) (sin a))) 0 life cool
      | i <- [1 .. count]
      , let a = 2 * pi * (fromIntegral i + wobble (tick * 7 + i)) / fromIntegral count ]

    grind =
      [ Particle p (Vec (3 * wobble (tick + i) - 1.5) (3 * wobble (tick * 3 + i) - 1.5)) 0 16 False
      | mode == Repaying, playing, tick `mod` 5 == 0
      , (i, p) <- zip [0 ..] (hazardPositions level (tsClock time1)) ]

    newSparks = concat
      [ if lost then burst 22 3.6 34 False else []
      , if won then burst 14 1.6 56 True else []
      , grind
      ]
    ageSpark p = p { ptPos = vAdd (ptPos p) (ptVel p), ptVel = vScale 0.94 (ptVel p), ptAge = ptAge p + 1 }
    liveSparks = filter (\p -> ptAge p < ptLife p) (fxParticles fx)
