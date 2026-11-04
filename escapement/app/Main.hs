-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   escapement                     play
--   escapement --reduced-effects   no trails, sparks or moving decoration
--   escapement --demo              watch the scripted solution, then take over
--   escapement --demo-stop=N       as --demo, but hold the picture after N ticks
--   escapement --fps               print measured frames per second to stdout
--   escapement --trace             print the game state to stdout every frame
module Main (main) where

import Control.Monad (when)
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Solution
import Game.Types
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss.Interface.IO.Game
import Render
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Art

main :: IO ()
main = do
  args <- getArgs
  let cfg = defaultConfig { cfgReducedEffects = "--reduced-effects" `elem` args }
      level = mkLevel cfg
      stopAt = listToMaybe (mapMaybe (fmap read . stripPrefix "--demo-stop=") args)
      demo = "--demo" `elem` args || stopAt /= Nothing
      scene = mkScene cfg level
      world0 = World
        { wGame    = initialGame cfg level
        , wFx      = initialFx
        , wStepper = Stepper 0 emptyInput
        , wWindow  = (1280, 720)
        , wScript  = if demo then solutionInputs else []
        , wStopAt  = stopAt
        , wTicks   = 0
        }
  start <- getMonotonicTime
  counter <- newIORef (start, 0 :: Int)
  let countFrame = when ("--fps" `elem` args) $ do
        now <- getMonotonicTime
        (since, frames) <- readIORef counter
        if now - since >= 1
          then do
            putStrLn ("fps " ++ show (fromIntegral (frames + 1) / (now - since)))
            hFlush stdout
            writeIORef counter (now, 0)
          else writeIORef counter (since, frames + 1)
  playIO
    (InWindow "Escapement" (1280, 720) (60, 40))
    Art.ink
    60
    (start, world0)
    (\(_, w) -> countFrame >> pure (render cfg level scene (wGame w) (wFx w) (wWindow w)))
    (\event (t, w) -> pure (t, onEvent event w))
    (\_ (t, w) -> do
        now <- getMonotonicTime
        let w' = onFrame cfg level (now - t) w
        when ("--trace" `elem` args) (putStrLn (traceLine level (wGame w')) >> hFlush stdout)
        pure (now, w'))

-- | One line of state for debugging and for driving the game from outside.
traceLine :: Level -> Game -> String
traceLine level game = unwords $
  [ show (gPhase game), show (gTicks game), show (tsClock time), show (tsMode time), show (tsDebt time) ]
  ++ coords (gPlayer game) ++ concatMap coords (hazardPositions level (tsClock time))
  where
    time = gTime game
    coords (Vec x y) = [show x, show y]

data World = World
  { wGame    :: !Game
  , wFx      :: !Fx
  , wStepper :: !Stepper
  , wWindow  :: !(Int, Int)
  , wScript  :: [TickInput]     -- ^ overrides the keyboard while it lasts
  , wStopAt  :: !(Maybe Int)
  , wTicks   :: !Int
  }

-- | Gloss's own idle loop catches up without limit after a stall, so its fixed
-- dt is ignored: the real clock goes through the clamped accumulator instead.
onFrame :: Config -> Level -> Double -> World -> World
onFrame cfg level elapsed w
  | Just stop <- wStopAt w, wTicks w >= stop = w
  | otherwise =
      let (stepper, (game, fx, script, ticks)) =
            advanceFrame cfg tick elapsed (wStepper w) (wGame w, wFx w, wScript w, wTicks w)
      in w { wGame = game, wFx = fx, wStepper = stepper, wScript = script, wTicks = ticks }
  where
    tick keyboard (game, fx, script, ticks) =
      let (input, script') = case script of
            (s : rest) -> (s, rest)
            []         -> (keyboard, [])
          game' = stepGame cfg level input game
          fx' = stepFx cfg level game game' fx
      in (inputMustClear input game game', (game', fx', script', ticks + 1))

onEvent :: Event -> World -> World
onEvent (EventResize size) w = w { wWindow = size }
onEvent (EventKey key state _ _) w = case gameKey key of
  Nothing -> w
  Just k  ->
    let edge = if state == Down then Press else Release
        stepper = wStepper w
    in w { wStepper = stepper { stInput = applyKey edge k (stInput stepper) } }
onEvent _ w = w

gameKey :: Key -> Maybe GameKey
gameKey key = case key of
  Char 'w'            -> Just GkW
  Char 'W'            -> Just GkW
  Char 'a'            -> Just GkA
  Char 'A'            -> Just GkA
  Char 's'            -> Just GkS
  Char 'S'            -> Just GkS
  Char 'd'            -> Just GkD
  Char 'D'            -> Just GkD
  Char 'p'            -> Just GkP
  Char 'P'            -> Just GkP
  Char 'r'            -> Just GkR
  Char 'R'            -> Just GkR
  Char ' '            -> Just GkSpace
  Char '\r'           -> Just GkEnter
  Char '\n'           -> Just GkEnter
  SpecialKey KeyUp    -> Just GkUp
  SpecialKey KeyDown  -> Just GkDown
  SpecialKey KeyLeft  -> Just GkLeft
  SpecialKey KeyRight -> Just GkRight
  SpecialKey KeySpace -> Just GkSpace
  SpecialKey KeyEnter -> Just GkEnter
  _                   -> Nothing
