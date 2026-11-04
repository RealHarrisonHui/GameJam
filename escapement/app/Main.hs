-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   escapement                     play from the title
--   escapement --level=N           start at floor N's note (1 to 5)
--   escapement --reduced-effects   no trails, sparks or moving decoration
--   escapement --no-hints          no teaching prompts or after-death hints, for playtests
--   escapement --no-ghost          no landing ghosts, for playtests
--   escapement --demo              watch the scripted run of every floor, then take over
--   escapement --demo-stop=N       as --demo, but hold the picture after N ticks
--   escapement --log               print one line per death, clear and pause to stdout
--   escapement --fps               print measured frames per second to stdout
--   escapement --trace             print the game state to stdout every frame
module Main (main) where

import Control.Monad (when)
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Game.Campaign
import Game.Fx
import Game.Input
import Game.Level
import Game.Solution
import Game.Types
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss.Interface.IO.Game
import Render
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Art

main :: IO ()
main = do
  args <- getArgs
  let cfg = defaultConfig
        { cfgReducedEffects = "--reduced-effects" `elem` args
        , cfgHints = "--no-hints" `notElem` args
        , cfgGhost = "--no-ghost" `notElem` args }
      levels = mkLevels cfg
      count = length levels
      scenes = [ mkScene cfg i count level | (i, level) <- zip [1 ..] levels ]
      argNumber name = listToMaybe (mapMaybe (fmap read . stripPrefix name) args) :: Maybe Int
      stopAt = argNumber "--demo-stop="
      demo = "--demo" `elem` args || stopAt /= Nothing
      start0 = case argNumber "--level=" of
        Just n  -> campaignAt cfg levels (n - 1)
        Nothing -> newCampaign cfg levels
      world0 = World
        { wCampaign = start0
        , wFx      = initialFx
        , wStepper = Stepper 0 emptyInput
        , wWindow  = (1280, 720)
        , wScript  = if demo then campaignInputs cfg levels else []
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
    (\(_, w) -> countFrame >> pure (render cfg levels scenes (wCampaign w) (wFx w) (wWindow w)))
    (\event (t, w) -> pure (t, onEvent event w))
    (\_ (t, w) -> do
        now <- getMonotonicTime
        let w' = onFrame cfg levels (now - t) w
        when ("--log" `elem` args) (logChange levels (wCampaign w) (wCampaign w'))
        when ("--trace" `elem` args) (putStrLn (traceLine levels (wCampaign w')) >> hFlush stdout)
        pure (now, w'))

-- | One line per event worth reading after a playtest.
logChange :: [Level] -> Campaign -> Campaign -> IO ()
logChange levels before after =
  when (cIndex before == cIndex after && cScreen after == InFloor && phase0 /= phase1 && phase1 `elem` [Lost, Won, Paused]) $ do
    let Vec x y = gPlayer game
        time = gTime game
    printf "%s floor=%d name=%s attempt-ticks=%d mode=%s debt=%d at=(%.0f,%.0f) deaths-here=%d\n"
      (show phase1) (cIndex after + 1) (show (lvlTitle (currentLevel levels after)))
      (gTicks game) (show (tsMode time)) (tsDebt time) x y (cDeaths after)
    hFlush stdout
  where
    game = cGame after
    phase0 = gPhase (cGame before)
    phase1 = gPhase game

-- | One line of state for debugging and for driving the game from outside.
traceLine :: [Level] -> Campaign -> String
traceLine levels c = unwords $
  [ show (cScreen c), show (cIndex c), show (gPhase game), show (gTicks game), show (tsClock time), show (tsMode time), show (tsDebt time) ]
  ++ coords (gPlayer game) ++ concatMap coords (hazardPositions (currentLevel levels c) (tsClock time))
  where
    game = cGame c
    time = gTime game
    coords (Vec x y) = [show x, show y]

data World = World
  { wCampaign :: !Campaign
  , wFx      :: !Fx
  , wStepper :: !Stepper
  , wWindow  :: !(Int, Int)
  , wScript  :: [TickInput]     -- ^ overrides the keyboard while it lasts
  , wStopAt  :: !(Maybe Int)
  , wTicks   :: !Int
  }

-- | Gloss's own idle loop catches up without limit after a stall, so its fixed
-- dt is ignored: the real clock goes through the clamped accumulator instead.
onFrame :: Config -> [Level] -> Double -> World -> World
onFrame cfg levels elapsed w
  | Just stop <- wStopAt w, wTicks w >= stop = w
  | otherwise =
      let (stepper, (campaign, fx, script, ticks)) =
            advanceFrame cfg tick elapsed (wStepper w) (wCampaign w, wFx w, wScript w, wTicks w)
      in w { wCampaign = campaign, wFx = fx, wStepper = stepper, wScript = script, wTicks = ticks }
  where
    tick keyboard (campaign, fx, script, ticks) =
      let (input, script') = case script of
            (s : rest) -> (s, rest)
            []         -> (keyboard, [])
          campaign' = stepCampaign cfg levels input campaign
          -- A new floor starts its effects clean, so no trail outlives its wheels.
          fx' | cIndex campaign /= cIndex campaign' || cScreen campaign /= cScreen campaign' =
                  initialFx { fxTick = fxTick fx + 1 }
              | otherwise =
                  stepFx cfg (currentLevel levels campaign') (cGame campaign) (cGame campaign') fx
      in (campaignMustClear input campaign campaign', (campaign', fx', script', ticks + 1))

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
