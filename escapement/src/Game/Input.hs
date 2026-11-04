-- | Turns key events and elapsed time into ticks. Pure; Main feeds it.
module Game.Input
  ( GameKey (..)
  , KeyEdge (..)
  , InputState
  , emptyInput
  , applyKey
  , tickInput
  , consumeOneShots
  , clearInput
  , Stepper (..)
  , advanceFrame
  ) where

import Game.Types

data GameKey
  = GkW | GkA | GkS | GkD
  | GkUp | GkDown | GkLeft | GkRight
  | GkSpace | GkP | GkR | GkEnter
  deriving (Eq, Show)

data KeyEdge = Press | Release deriving (Eq, Show)

data InputState = InputState
  { inHeld           :: [GameKey]
  , inFreezePressed  :: !Bool
  , inPausePressed   :: !Bool
  , inRestartPressed :: !Bool
  , inConfirmPressed :: !Bool
  } deriving (Eq, Show)

emptyInput :: InputState
emptyInput = InputState [] False False False False

-- | Only a transition from up to down counts as a press, so OS key repeat is harmless.
applyKey :: KeyEdge -> GameKey -> InputState -> InputState
applyKey Release key input = input { inHeld = filter (/= key) (inHeld input) }
applyKey Press key input
  | key `elem` inHeld input = input
  | otherwise =
      let held = input { inHeld = key : inHeld input }
      in case key of
           GkSpace -> held { inFreezePressed = True }
           GkP     -> held { inPausePressed = True }
           GkR     -> held { inRestartPressed = True }
           GkEnter -> held { inConfirmPressed = True }
           _        -> held

tickInput :: InputState -> TickInput
tickInput input = TickInput
  { tiMoveX          = axis [GkD, GkRight] - axis [GkA, GkLeft]
  , tiMoveY          = axis [GkW, GkUp] - axis [GkS, GkDown]
  , tiFreezeHeld     = GkSpace `elem` inHeld input
  , tiFreezePressed  = inFreezePressed input
  , tiPausePressed   = inPausePressed input
  , tiRestartPressed = inRestartPressed input
  , tiConfirmPressed = inConfirmPressed input
  }
  where
    axis keys = if any (`elem` inHeld input) keys then 1 else 0

-- | Called after every tick, so a press reaches exactly one tick however many
-- catch-up ticks a frame runs.
consumeOneShots :: InputState -> InputState
consumeOneShots input = input
  { inFreezePressed = False, inPausePressed = False
  , inRestartPressed = False, inConfirmPressed = False }

-- | Forget held keys. A key still physically down must be pressed again.
clearInput :: InputState -> InputState
clearInput _ = emptyInput

-- | The fixed-step accumulator's state.
data Stepper = Stepper
  { stAccumulator :: !Double
  , stInput       :: !InputState
  } deriving (Eq, Show)

-- | Run as many fixed ticks as the elapsed time pays for. A long frame is
-- clamped, the tick count is bounded, and time beyond the bound is dropped
-- rather than remembered. @tick@ reports whether held input must be cleared,
-- which is how screen changes and restarts reach this layer.
advanceFrame
  :: Config
  -> (TickInput -> s -> (Bool, s))
  -> Double
  -> Stepper
  -> s
  -> (Stepper, s)
advanceFrame cfg tick elapsed stepper state0 =
  let dt = 1 / fromIntegral (cfgTicksPerSecond cfg)
      total = stAccumulator stepper + max 0 (min (cfgMaxFrameTime cfg) elapsed)
      wanted = floor (total / dt) :: Int
      count = min (cfgMaxCatchUp cfg) wanted
      left = if wanted > count then 0 else total - fromIntegral count * dt
      go 0 input s = (input, s)
      go n input s =
        let (clear, s') = tick (tickInput input) s
            input' = if clear then clearInput input else consumeOneShots input
        in go (n - 1) input' s'
      (inputEnd, stateEnd) = go count (stInput stepper) state0
  in (Stepper left inputEnd, stateEnd)
