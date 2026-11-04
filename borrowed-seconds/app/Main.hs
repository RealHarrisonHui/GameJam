-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   borrowed-seconds                play
--   borrowed-seconds --demo         watch the verified route play itself, then take over
--   borrowed-seconds --reduced-fx   no particles, trails, glows or turning wheel; same rules
--
-- Development flags, for inspecting exact moments:
--   --plan=intended|none|one|late|indoor   the scripted play for --at (default intended)
--   --at=N                          run that plan for N ticks of input, then freeze
--   --idle=N                        then N more ticks with no keys, then freeze
--   --pause                         then pause, let the panel settle, then freeze
--   --title=N                       freeze the title screen at effect tick N
--   --size=WxH                      open the window at this size
--   --fps                           print measured frames per second
--   --trace                         print state whenever the phase or swap count changes
module Main (main) where

import Control.Monad (when)
import Data.Foldable (foldl')
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Game.Clock
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Route
import Game.Types
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game
import Platform (exitWhenWindowCloses)
import Render
import Scene (mkScene)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data World = World
  { wGame   :: !GameState
  , wFx     :: !Fx
  , wKeys   :: !Keys
  , wWindow :: !(Int, Int)
  , wAcc    :: !Accumulator
  , wLast   :: !Double       -- ^ Monotonic time of the previous frame.
  , wScript :: [TickInput]   -- ^ Overrides the keyboard while it lasts.
  , wFrozen :: !Bool         -- ^ Inspection only: no ticks at all.
  }

-- | 1280 x 720, shrunk to fit a smaller display.
windowFor :: (Int, Int) -> (Int, Int)
windowFor (screenW, screenH)
  | screenW <= 0 || screenH <= 0 = (1280, 720)
  | otherwise = (round (1280 * k), round (720 * k))
  where
    margin = 90 :: Float  -- title bar and menu bar
    k = minimum [1, (fromIntegral screenW - 20) / 1280, (fromIntegral screenH - margin) / 720]

planNamed :: String -> Plan
planNamed name = case name of
  "none" -> []
  "one"  -> [AtTick 6]
  "late" -> [AtTick 150, ClearOf DoorA]
  "indoor" -> [AtTick 6, InDoor DoorA]
  _      -> intendedPlan

main :: IO ()
main = do
  args <- getArgs
  let level = theLevel
      flag name = listToMaybe (mapMaybe (stripPrefix ("--" ++ name ++ "=")) args)
      has name = ("--" ++ name) `elem` args
      style = if has "reduced-fx" then Reduced else Full
      planName = maybe "intended" id (flag "plan")
      atTick = fmap read (flag "at") :: Maybe Int
      idle = maybe 0 read (flag "idle") :: Int
      titleAt = fmap read (flag "title") :: Maybe Int
      planned = planInputs level 3000 (planNamed planName)
      prepared = case (atTick, titleAt) of
        (_, Just n) -> replicate n noInput
        (Just n, _) -> take n planned ++ replicate idle noInput
                         ++ concat [ noInput { tiPause = True } : replicate 40 noInput | has "pause" ]
        _ -> []
      (g0, fx0) = foldl' (tickBoth level) (initialGame level, initialFx level) prepared
      inspect = isJust atTick || isJust titleAt
      scene = mkScene level
  screen <- getScreenSize
  let windowSize = case flag "size" of
        Just s | (w, 'x' : h) <- break (== 'x') s -> (read w, read h)
        _ -> windowFor screen
  start <- getMonotonicTime
  counter <- newIORef (start, 0 :: Int)
  firstFrame <- newIORef True
  let world0 = World
        { wGame = g0, wFx = fx0, wKeys = noKeys, wWindow = windowSize
        , wAcc = emptyAccumulator, wLast = start
        , wScript = if has "demo" then replicate 45 noInput ++ planned else []
        , wFrozen = inspect
        }
      countFrame = when (has "fps") $ do
        now <- getMonotonicTime
        (since, frames) <- readIORef counter
        if now - since >= 1
          then do
            putStrLn ("fps " ++ show (fromIntegral (frames + 1) / (now - since) :: Double))
            hFlush stdout
            writeIORef counter (now, 0)
          else writeIORef counter (since, frames + 1)
  playIO
    (InWindow "Borrowed Seconds" windowSize (40, 40))
    black
    60
    world0
    (\w -> do
        first <- readIORef firstFrame
        when first (writeIORef firstFrame False >> exitWhenWindowCloses)
        countFrame
        pure (render style scene level (wGame w) (wFx w) (wWindow w)))
    (\event w -> pure (onEvent event w))
    (\_ w -> do
        now <- getMonotonicTime
        let w' = onFrame level now w
        when (has "trace" && traceKey (wGame w') /= traceKey (wGame w)) $ do
          let g = wGame w'
          putStrLn (show (gsPhase g) ++ " tick=" ++ show (gsTick g) ++ " swaps=" ++ show (gsSwaps g)
                    ++ " " ++ show (gsTimers g) ++ " pos=" ++ show (psPos (gsPlayer g)))
          hFlush stdout
        pure w')
  where
    traceKey g = (gsPhase g, gsSwaps g, gsTick g == 0)

tickBoth :: Level -> (GameState, Fx) -> TickInput -> (GameState, Fx)
tickBoth level (g, fx) input =
  let g' = stepGame level input g in (g', stepFx level g g' fx)

onFrame :: Level -> Double -> World -> World
onFrame level now w
  | wFrozen w = w
  | otherwise = iterate (oneTick level) w { wAcc = acc, wLast = now } !! ticks
  where (ticks, acc) = accumulate (now - wLast w) (wAcc w)

oneTick :: Level -> World -> World
oneTick level w =
  let (input, script) = case wScript w of
        (i : rest) -> (i, rest)
        []         -> (tickInput (wKeys w), [])
      (g, fx) = tickBoth level (wGame w, wFx w) input
  in w { wGame = g, wFx = fx, wScript = script, wKeys = afterTick (wGame w) g (wKeys w) }

onEvent :: Event -> World -> World
onEvent (EventResize size) w = w { wWindow = size }
onEvent (EventKey key Down _ _) w = withKey key keyDown w
onEvent (EventKey key Up _ _) w = withKey key keyUp w
onEvent _ w = w

withKey :: Key -> (GameKey -> Keys -> Keys) -> World -> World
withKey key f w = case gameKey key of
  Just k  -> w { wKeys = f k (wKeys w) }
  Nothing -> w

gameKey :: Key -> Maybe GameKey
gameKey key = case key of
  Char 'w' -> Just KUp
  Char 'W' -> Just KUp
  Char 's' -> Just KDown
  Char 'S' -> Just KDown
  Char 'a' -> Just KLeft
  Char 'A' -> Just KLeft
  Char 'd' -> Just KRight
  Char 'D' -> Just KRight
  Char 'p' -> Just KPause
  Char 'P' -> Just KPause
  Char 'r' -> Just KRestart
  Char 'R' -> Just KRestart
  Char ' ' -> Just KSpace
  Char '\r' -> Just KEnter
  Char '\n' -> Just KEnter
  SpecialKey KeySpace -> Just KSpace
  SpecialKey KeyEnter -> Just KEnter
  SpecialKey KeyUp    -> Just KUp
  SpecialKey KeyDown  -> Just KDown
  SpecialKey KeyLeft  -> Just KLeft
  SpecialKey KeyRight -> Just KRight
  _ -> Nothing

