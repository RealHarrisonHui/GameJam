{-# LANGUAGE BangPatterns #-}
-- | A tiny software painter. A picture is a pure function from a pixel position
-- to a colour, which is the same idea as a GPU fragment shader. We run it once
-- at startup and hand the finished images to Gloss.
--
-- Gloss has no anti-aliasing, gradients or shaders, but it can draw an image, so
-- everything Gloss cannot do is painted here instead.
module Art.Raster
  ( -- * Colour
    C3 (..)
  , RGBA (..)
  , hex
  , mix3
  , scale3
  , tint
  , clear
  , over
  , layers
    -- * Maths
  , clamp01
  , smoothstep
  , mixF
  , hash2
  , cover
    -- * Shapes, as signed distances (negative is inside)
  , sdCircle
  , sdBox
  , sdSegment
    -- * Sprites
  , Shader
  , Sprite (..)
  , bake
  ) where

import Data.Bits (shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.List (foldl')
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

import Graphics.Gloss.Data.Bitmap (BitmapFormat (..), PixelFormat (..), RowOrder (..), bitmapOfByteString)
import Graphics.Gloss.Data.Picture (Picture)

data C3 = C3 !Float !Float !Float

-- | Red, green, blue and alpha, with the colour already multiplied by alpha.
data RGBA = RGBA !Float !Float !Float !Float

hex :: Int -> C3
hex n = C3 (channel 16) (channel 8) (channel 0)
  where channel s = fromIntegral ((n `shiftR` s) `mod` 256) / 255

mix3 :: Float -> C3 -> C3 -> C3
mix3 t (C3 a b c) (C3 x y z) = C3 (mixF t a x) (mixF t b y) (mixF t c z)

scale3 :: Float -> C3 -> C3
scale3 k (C3 a b c) = C3 (k * a) (k * b) (k * c)

tint :: C3 -> Float -> RGBA
tint (C3 r g b) a = RGBA (r * a) (g * a) (b * a) a
{-# INLINE tint #-}

clear :: RGBA
clear = RGBA 0 0 0 0

-- | Paint the first colour on top of the second.
over :: RGBA -> RGBA -> RGBA
over (RGBA r g b a) (RGBA x y z w) =
  let k = 1 - a in RGBA (r + x * k) (g + y * k) (b + z * k) (a + w * k)
{-# INLINE over #-}

-- | Stack colours, listed from bottom to top.
layers :: [RGBA] -> RGBA
layers = foldl' (flip over) clear

clamp01 :: Float -> Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
{-# INLINE clamp01 #-}

smoothstep :: Float -> Float -> Float -> Float
smoothstep a b x = let t = clamp01 ((x - a) / (b - a)) in t * t * (3 - 2 * t)
{-# INLINE smoothstep #-}

mixF :: Float -> Float -> Float -> Float
mixF t a b = a + (b - a) * t
{-# INLINE mixF #-}

-- | A repeatable random number in 0..1 for a pair of whole numbers.
hash2 :: Int -> Int -> Float
hash2 x y =
  let a = x * 73856093 `xor` y * 19349663
      b = (a `xor` (a `shiftR` 13)) * 1274126177
      c = b `xor` (b `shiftR` 16)
  in fromIntegral (c `mod` 65536) / 65535

-- | How much of a pixel a shape covers, given the distance to its edge. This
-- one line is what gives every shape a smooth edge.
cover :: Float -> Float
cover d = clamp01 (0.5 - d)
{-# INLINE cover #-}

sdCircle :: Float -> Float -> Float -> Float -> Float -> Float
sdCircle cx cy r x y = sqrt ((x - cx) * (x - cx) + (y - cy) * (y - cy)) - r
{-# INLINE sdCircle #-}

-- | A rounded box, from its centre, half-width, half-height and corner radius.
sdBox :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
sdBox cx cy hw hh r x y =
  let qx = abs (x - cx) - hw + r
      qy = abs (y - cy) - hh + r
      mx = max qx 0
      my = max qy 0
  in sqrt (mx * mx + my * my) + min (max qx qy) 0 - r
{-# INLINE sdBox #-}

-- | Distance to a line segment.
sdSegment :: Float -> Float -> Float -> Float -> Float -> Float -> Float
sdSegment ax ay bx by x y =
  let px = x - ax
      py = y - ay
      dx = bx - ax
      dy = by - ay
      lengthSquared = dx * dx + dy * dy
      h = if lengthSquared > 0 then clamp01 ((px * dx + py * dy) / lengthSquared) else 0
      ex = px - dx * h
      ey = py - dy * h
  in sqrt (ex * ex + ey * ey)
{-# INLINE sdSegment #-}

-- | Maps a pixel centre to a colour.
type Shader = Float -> Float -> RGBA

-- | A finished image, placed relative to an anchor point.
data Sprite = Sprite
  { spriteX      :: !Int
  , spriteY      :: !Int
  , spriteW      :: !Int
  , spriteH      :: !Int
  , spritePixels :: !B.ByteString
  , spriteImage  :: Picture
  }

-- | Paint a rectangle (left, top, width, height). Sizes are rounded up to even
-- numbers, because Gloss centres images and an odd size would blur by half a
-- pixel.
bake :: (Int, Int, Int, Int) -> Shader -> Sprite
bake (x0, y0, w0, h0) shader = Sprite
  { spriteX = x0
  , spriteY = y0
  , spriteW = w
  , spriteH = h
  , spritePixels = pixels
  , spriteImage = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) pixels True
  }
  where
    w = w0 + w0 `mod` 2
    h = h0 + h0 `mod` 2
    pixels = BI.unsafeCreate (w * h * 4) (fillRows 0)

    fillRows :: Int -> Ptr Word8 -> IO ()
    fillRows !j !p
      | j >= h    = pure ()
      | otherwise = fillRow j 0 p >> fillRows (j + 1) (p `plusPtr` (w * 4))

    fillRow :: Int -> Int -> Ptr Word8 -> IO ()
    fillRow !j !i !p
      | i >= w    = pure ()
      | otherwise = do
          let px = x0 + i
              py = y0 + j
              -- A little noise hides the visible bands in slow, dark gradients.
              noise = (hash2 px py - 0.5) / 255
              RGBA r g b a = shader (fromIntegral px + 0.5) (fromIntegral py + 0.5)
              straight v = if a > 0.0005 then v / a else 0
          poke p (toByte (straight r + noise))
          poke (p `plusPtr` 1) (toByte (straight g + noise))
          poke (p `plusPtr` 2) (toByte (straight b + noise))
          poke (p `plusPtr` 3) (toByte (a + noise))
          fillRow j (i + 1) (p `plusPtr` 4)

toByte :: Float -> Word8
toByte v = round (clamp01 v * 255)
{-# INLINE toByte #-}
