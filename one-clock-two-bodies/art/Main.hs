-- | The art test: "The Stopped Hour". It plays the level's real solution as a
-- demo, and the first game key you press hands the controls over to you.
--
-- > art-test               painted scene, in a window
-- > art-test --play        painted scene, skipping the demo
-- > art-test --plain       the same scene with plain Gloss shapes
-- > art-test --measure     painted scene, printing frames per second
-- > art-test --still T F   write the frame at T seconds to the PPM file F
-- > art-test --stream      write one demo loop of 30 fps PPM frames to stdout
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, peekByteOff, poke)
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO (hPutStrLn, hSetBinaryMode, stderr, stdout)

import Graphics.Gloss.Interface.IO.Game

import Game.Types

import Art.Plain
import Art.Raster (Sprite (..))
import Art.Scene
import Art.Visual

frameTime :: Float
frameTime = 1 / 30

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--plain"]       -> runWindow "The Stopped Hour (plain shapes)" False demoWorld plainPicture
    ["--play"]        -> withAssets $ \assets ->
      runWindow "The Stopped Hour" False playerWorld (renderItems . drawWorld assets)
    ["--measure"]     -> withAssets $ \assets ->
      runWindow "The Stopped Hour" True demoWorld (renderItems . drawWorld assets)
    ["--still", t, f] -> withAssets $ \assets -> writeStill assets (read t) f
    ["--stream"]      -> withAssets streamFrames
    _                 -> withAssets $ \assets ->
      runWindow "The Stopped Hour" False demoWorld (renderItems . drawWorld assets)

-- | Paint everything up front and report what it cost.
withAssets :: (Assets -> IO ()) -> IO ()
withAssets continue = do
  start <- getMonotonicTime
  let assets = makeAssets
      sprites = allSprites assets
  forM_ sprites evaluate
  end <- getMonotonicTime
  let bytes = sum (map (B.length . spritePixels) sprites)
  hPutStrLn stderr $
    "Painted " ++ show (length sprites) ++ " images, "
      ++ show (bytes `div` (1024 * 1024)) ++ " MB, in "
      ++ show (fromIntegral (round ((end - start) * 100) :: Int) / (100 :: Double)) ++ " s"
  continue assets

runWindow :: String -> Bool -> World -> (World -> Picture) -> IO ()
runWindow title measuring start render = do
  frames <- newIORef (0 :: Int)
  started <- getMonotonicTime >>= newIORef
  let countFrame = when measuring $ do
        modifyIORef' frames (+ 1)
        now <- getMonotonicTime
        since <- readIORef started
        when (now - since >= 1) $ do
          count <- readIORef frames
          hPutStrLn stderr (show count ++ " frames per second")
          writeIORef frames 0
          writeIORef started now
  playIO
    (InWindow title (sceneWidth, sceneHeight) (100, 100))
    black
    30
    start
    (\world -> countFrame >> pure (render world))
    (\event world -> pure (handleEvent event world))
    (\dt world -> pure (stepWorld dt world))

handleEvent :: Event -> World -> World
handleEvent (EventKey key Down _ _) world = maybe world (`pressKey` world) (keyToAction key)
handleEvent (EventKey key Up _ _) world = maybe world (`releaseKey` world) (keyToAction key)
handleEvent _ world = world

keyToAction :: Key -> Maybe Action
keyToAction (SpecialKey KeyUp)    = Just (Move DirUp)
keyToAction (SpecialKey KeyDown)  = Just (Move DirDown)
keyToAction (SpecialKey KeyLeft)  = Just (Move DirLeft)
keyToAction (SpecialKey KeyRight) = Just (Move DirRight)
keyToAction (SpecialKey KeyTab)   = Just SwitchTime
keyToAction (Char c) = case toLower c of
  'w' -> Just (Move DirUp)
  's' -> Just (Move DirDown)
  'a' -> Just (Move DirLeft)
  'd' -> Just (Move DirRight)
  'r' -> Just Restart
  _   -> Nothing
keyToAction _ = Nothing

-- Drawing with Gloss.

renderItems :: [Item] -> Picture
renderItems = pictures . map toPicture
  where
    toPicture (Fade a) = color (withAlpha a black) (rectangleSolid 960 600)
    toPicture (Blit sprite x y) =
      translate
        (fromIntegral (x + spriteX sprite) + fromIntegral (spriteW sprite) / 2 - 480)
        (300 - fromIntegral (y + spriteY sprite) - fromIntegral (spriteH sprite) / 2)
        (spriteImage sprite)

-- Drawing without a window, for stills and video. This blends the same images
-- by hand, so the output needs no screen recording.

worldAt :: Float -> World
worldAt seconds =
  iterate (stepWorld frameTime) demoWorld !! round (seconds / frameTime)

writeStill :: Assets -> Float -> FilePath -> IO ()
writeStill assets seconds file =
  B.writeFile file (ppmFrame (drawWorld assets (worldAt seconds)))

streamFrames :: Assets -> IO ()
streamFrames assets = do
  hSetBinaryMode stdout True
  let frameCount = round (loopLength / frameTime) :: Int
      worlds = take frameCount (iterate (stepWorld frameTime) demoWorld)
  forM_ worlds (B.hPut stdout . ppmFrame . drawWorld assets)

ppmFrame :: [Item] -> B.ByteString
ppmFrame items =
  BC.pack ("P6\n" ++ show sceneWidth ++ " " ++ show sceneHeight ++ "\n255\n")
    <> BI.unsafeCreate (sceneWidth * sceneHeight * 3) paint
  where
    paint p = do
      forM_ [0 .. sceneWidth * sceneHeight * 3 - 1] $ \i -> poke (p `plusPtr` i) (0 :: Word8)
      forM_ items (drawItem p)

drawItem :: Ptr Word8 -> Item -> IO ()
drawItem p (Fade a) =
  forM_ [0 .. sceneWidth * sceneHeight * 3 - 1] $ \i -> do
    v <- peek (p `plusPtr` i) :: IO Word8
    poke (p `plusPtr` i) (round (fromIntegral v * (1 - a)) :: Word8)
drawItem p (Blit sprite x y) =
  BU.unsafeUseAsCString (spritePixels sprite) $ \source ->
    forM_ [0 .. spriteH sprite - 1] $ \j ->
      forM_ [0 .. spriteW sprite - 1] $ \i -> do
        let px = x + spriteX sprite + i
            py = y + spriteY sprite + j
            from = (j * spriteW sprite + i) * 4
        when (px >= 0 && px < sceneWidth && py >= 0 && py < sceneHeight) $ do
          alpha <- peekByteOff source (from + 3) :: IO Word8
          when (alpha > 0) $
            forM_ [0, 1, 2] $ \c -> do
              top <- peekByteOff source (from + c) :: IO Word8
              let target = p `plusPtr` ((py * sceneWidth + px) * 3 + c)
              below <- peek target :: IO Word8
              poke target (blend alpha top below)

blend :: Word8 -> Word8 -> Word8 -> Word8
blend alpha top below =
  let a = fromIntegral alpha / 255 :: Float
  in round (fromIntegral top * a + fromIntegral below * (1 - a))
