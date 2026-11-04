-- | Decorative state, derived by comparing one game state with the next. Pure, but
-- nothing in the rules can read it: gameplay is identical with effects on or off.
--
-- Everything here is bounded, and everything is wiped when an attempt starts.
module Game.Fx
  ( Fx (..)
  , Capsule (..)
  , Particle (..)
  , ParticleKind (..)
  , initialFx
  , stepFx
  , capsuleLife
  , maxParticles
  , maxTrail
  , maxCapsules
  , attractPeriod
  ) where

import Game.Logic (isOpen, startLevel)
import Game.Types

-- | One exchange: two capsules cross in the tube, carrying what each door received.
data Capsule = Capsule
  { capAge :: !Int
  , capToA :: !Int  -- ^ Ticks A was given by this exchange.
  , capToB :: !Int  -- ^ Ticks B was given by this exchange.
  } deriving (Eq, Show)

data ParticleKind
  = Spark !DoorId   -- ^ Thrown off a dial by an exchange.
  | Soot            -- ^ Knocked from a door as it seals.
  | Gilt            -- ^ Celebration at the exit.
  deriving (Eq, Show)

data Particle = Particle
  { ptPos  :: !Vec
  , ptVel  :: !Vec
  , ptAge  :: !Int
  , ptLife :: !Int
  , ptKind :: !ParticleKind
  } deriving (Eq, Show)

data Fx = Fx
  { fxClock     :: !Int       -- ^ Ticks since launch. Never stops: for idle motion.
  , fxPhaseAge  :: !Int       -- ^ Ticks since the screen last changed.
  , fxCapsules  :: [Capsule]  -- ^ Newest first.
  , fxGateA     :: !Double    -- ^ How far door A's leaves are drawn back, 0 to 1.
  , fxGateB     :: !Double
  , fxPopA      :: !Double    -- ^ Dial pulse after a change, falling from 1 to 0.
  , fxPopB      :: !Double
  , fxSealA     :: !Double    -- ^ Impact when the door shuts, falling from 1 to 0.
  , fxSealB     :: !Double
  , fxTrail     :: [Vec]      -- ^ Newest first.
  , fxStride    :: !Double    -- ^ Distance walked: the feet's phase.
  , fxHeading   :: !Double    -- ^ Eased facing, in radians, y down.
  , fxParticles :: [Particle]
  , fxSeed      :: !Int
  } deriving (Eq, Show)

capsuleLife, maxParticles, maxTrail, maxCapsules :: Int
capsuleLife = 40
maxParticles = 90
maxTrail = 14
maxCapsules = 3

-- | The title screen's demonstration exchanges this often, in ticks.
attractPeriod :: Int
attractPeriod = 170

initialFx :: Level -> Fx
initialFx _ = Fx
  { fxClock = 0
  , fxPhaseAge = 0
  , fxCapsules = []
  , fxGateA = 1
  , fxGateB = 1
  , fxPopA = 0
  , fxPopB = 0
  , fxSealA = 0
  , fxSealB = 0
  , fxTrail = []
  , fxStride = 0
  , fxHeading = 0
  , fxParticles = []
  , fxSeed = 20261104
  }

stepFx :: Level -> GameState -> GameState -> Fx -> Fx
stepFx level before after fx
  | begun = (initialFx level)
      { fxClock = fxClock fx + 1
      , fxPhaseAge = 0
      , fxSeed = fxSeed fx
      }
  | otherwise = case gsPhase after of
      Paused -> ticked   -- the scene holds still; only UI easing moves
      Title -> attract ticked
      Playing -> living level before after ticked
      _ -> settling level before after ticked
  where
    begun = before /= after && after { gsSwapHeld = False } == startLevel level
    phaseChanged = gsPhase before /= gsPhase after
    ticked = fx
      { fxClock = fxClock fx + 1
      , fxPhaseAge = if phaseChanged then 0 else fxPhaseAge fx + 1
      }

-- | The title screen's attract loop just needs its capsules aged, and a new pair
-- launched each period.
attract :: Fx -> Fx
attract fx
  | fxClock fx `mod` attractPeriod == attractPeriod `div` 2 =
      aged { fxCapsules = take maxCapsules (Capsule 0 (held DoorB) (held DoorA) : fxCapsules aged) }
  | otherwise = aged
  where
    aged = fx { fxCapsules = ageCapsules (fxCapsules fx) }
    -- Which pair the attract dials hold before this exchange.
    swapsSoFar = fxClock fx `div` attractPeriod
    held door = case (even swapsSoFar, door) of
      (True, DoorA)  -> 120
      (True, DoorB)  -> 720
      (False, DoorA) -> 720
      (False, DoorB) -> 120

ageCapsules :: [Capsule] -> [Capsule]
ageCapsules cs = [ c { capAge = capAge c + 1 } | c <- cs, capAge c + 1 < capsuleLife ]

living :: Level -> GameState -> GameState -> Fx -> Fx
living level before after fx0 =
  decay
    . gates after
    . sealImpacts level before after
    . walking before after
    . stepParticles
    . swapped level before after
    $ fx0

-- | Won or lost: the world stops, but what is in the air finishes, and the exit
-- celebrates on a win.
settling :: Level -> GameState -> GameState -> Fx -> Fx
settling level before after fx0 =
  decay . gates after . celebrate . stepParticles $ fx0 { fxPhaseAge = fxPhaseAge fx0 }
  where
    celebrate fx
      | gsPhase after == Won && gsPhase before /= Won = burst level 44 fx
      | gsPhase after == Won && fxPhaseAge fx < 150 && fxPhaseAge fx `mod` 9 == 0 = burst level 3 fx
      | otherwise = fx

swapped :: Level -> GameState -> GameState -> Fx -> Fx
swapped _ before after fx
  | gsSwaps after <= gsSwaps before = fx { fxCapsules = ageCapsules (fxCapsules fx) }
  | otherwise =
      -- Timers were swapped, then ticked. Undo the tick for display of the amounts.
      let DoorTimers a0 b0 = gsTimers before
          capsule = Capsule 0 b0 a0
          sparks = sparksAt DoorA dialACentre fx ++ sparksAt DoorB dialBCentre fx { fxSeed = fxSeed fx + 97 }
      in fx
        { fxCapsules = take maxCapsules (capsule : ageCapsules (fxCapsules fx))
        , fxPopA = 1
        , fxPopB = 1
        , fxParticles = bound (sparks ++ fxParticles fx)
        , fxSeed = fxSeed fx + 193
        }
  where
    dialACentre = (730, 300)
    dialBCentre = (360, 300)

sparksAt :: DoorId -> Vec -> Fx -> [Particle]
sparksAt door (cx, cy) fx =
  [ Particle (cx + 62 * cos a, cy + 62 * sin a) (speed * cos a, speed * sin a) 0 life (Spark door)
  | i <- [0 .. 11 :: Int]
  , let r k = noise (fxSeed fx + i * 7 + k)
        a = 2 * pi * (fromIntegral i + r 1) / 12
        speed = 0.5 + 0.9 * r 2
        life = 16 + round (14 * r 3)
  ]

walking :: GameState -> GameState -> Fx -> Fx
walking before after fx =
  let (x0, y0) = psPos (gsPlayer before)
      (x1, y1) = psPos (gsPlayer after)
      moved = sqrt ((x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int))
      (fx', fy') = psFacing (gsPlayer after)
      target = atan2 fy' fx'
      turn = wrapAngle (target - fxHeading fx)
      trail
        | moved > 0.01 = take maxTrail ((x1, y1) : fxTrail fx)
        | otherwise = take (max 0 (length (fxTrail fx) - 1)) (fxTrail fx)
  in fx
    { fxTrail = trail
    , fxStride = fxStride fx + moved
    , fxHeading = fxHeading fx + 0.28 * turn
    }

wrapAngle :: Double -> Double
wrapAngle a
  | a > pi = wrapAngle (a - 2 * pi)
  | a < negate pi = wrapAngle (a + 2 * pi)
  | otherwise = a

gates :: GameState -> Fx -> Fx
gates g fx = fx
  { fxGateA = ease DoorA (fxGateA fx)
  , fxGateB = ease DoorB (fxGateB fx)
  }
  where
    -- Fast: the drawn door may trail the rule by a moment, and no more.
    -- A door held by a player standing in it stays drawn back until they leave.
    ease door v
      | not (shutFor door g) = min 1 (v + 0.12)
      | otherwise = max 0 (v - 0.2)

sealImpacts :: Level -> GameState -> GameState -> Fx -> Fx
sealImpacts level before after fx = foldl impact fx allDoors
  where
    impact f door
      | not (shutFor door before) && shutFor door after =
          let f' = setSeal door 1 f
              (Rect l t r b) = doorRect level door
              cx = (l + r) / 2
              soot =
                [ Particle (cx, t + (b - t) * r1) (1.1 * (r2 - 0.5), 1.1 * (r3 - 0.5)) 0 (20 + round (14 * r4)) Soot
                | i <- [0 .. 9 :: Int]
                , let n k = noise (fxSeed f + i * 11 + k)
                      r1 = n 1
                      r2 = n 2
                      r3 = n 3
                      r4 = n 4
                ]
          in f' { fxParticles = bound (soot ++ fxParticles f'), fxSeed = fxSeed f' + 151 }
      | otherwise = f

-- | Shut as the player sees it: no time, and nobody standing in the way.
shutFor :: DoorId -> GameState -> Bool
shutFor door g = not (isOpen door (gsTimers g)) && not (occupiedBy door (gsOccupancy g))

setSeal :: DoorId -> Double -> Fx -> Fx
setSeal DoorA v fx = fx { fxSealA = v }
setSeal DoorB v fx = fx { fxSealB = v }

decay :: Fx -> Fx
decay fx = fx
  { fxPopA = fall 0.045 (fxPopA fx)
  , fxPopB = fall 0.045 (fxPopB fx)
  , fxSealA = fall 0.06 (fxSealA fx)
  , fxSealB = fall 0.06 (fxSealB fx)
  }
  where fall k v = max 0 (v - k)

stepParticles :: Fx -> Fx
stepParticles fx = fx { fxParticles = [ move p | p <- fxParticles fx, ptAge p + 1 < ptLife p ] }
  where
    move p =
      let (x, y) = ptPos p
          (vx, vy) = ptVel p
          drag = 0.97
      in p { ptPos = (x + vx, y + vy), ptVel = (vx * drag, vy * drag), ptAge = ptAge p + 1 }

burst :: Level -> Int -> Fx -> Fx
burst level n fx = fx
  { fxParticles = bound (new ++ fxParticles fx)
  , fxSeed = fxSeed fx + 211
  }
  where
    (cx, cy) = lvExit level
    new =
      [ Particle (cx, cy) (speed * cos a, speed * sin a - 0.3) 0 life Gilt
      | i <- [0 .. n - 1]
      , let r k = noise (fxSeed fx + i * 13 + k)
            a = 2 * pi * r 1
            speed = 0.8 + 3 * r 2
            life = 30 + round (40 * r 3)
      ]

bound :: [Particle] -> [Particle]
bound = take maxParticles

-- | Repeatable noise in [0,1).
noise :: Int -> Double
noise v =
  let a = (v * 1597334677 + 12345) `mod` 2147483647
      b = (a * 48271) `mod` 2147483647
      c = (b * 48271) `mod` 2147483647
  in fromIntegral c / 2147483647
