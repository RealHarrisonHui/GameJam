-- | Things that move in the world: doors, the courier, the exit's light, particles.
module Figures
  ( doorLive
  , courier
  , trail
  , particles
  , exitLive
  ) where

import Draw
import Font
import Game.Fx
import Game.Types
import Graphics.Gloss

-- | One door. Its thickness and span are exactly the collision rectangle, so what you
-- see blocking you is what blocks you. @gate@ is how far its leaves are drawn back.
doorLive :: Bool -> DoorId -> Rect -> Int -> Bool -> Float -> Float -> Int -> Picture
doorLive reduced door (Rect l t r b) ticks held gate seal clock = Pictures $
  [ wash | not reduced && open ]
  ++ [ threshold | gate > 0.5 ]
  ++ [ leaf y0 (y0 + leafLen) 1 , leaf (y1 - leafLen) y1 (-1) ]
  ++ [ sealedFace | gate < 0.02 ]
  ++ [ heldMark | held ]
  ++ [ slam | not reduced && seal > 0 ]
  where
    x0 = realToFrac l :: Float
    x1 = realToFrac r :: Float
    y0 = realToFrac t :: Float
    y1 = realToFrac b :: Float
    cx = (x0 + x1) / 2
    cy = (y0 + y1) / 2
    open = ticks > 0
    leafLen = (y1 - y0) / 2 * (1 - gate) + 3
    tone = doorColor door
    -- A translucent wash of the door's colour across the floor while it is open.
    wash = Pictures
      [ Color (fade 0.09 tone) (wRect (cx - w) (y0 + 1) (cx + w) (y1 - 1)) | w <- [12, 22, 34] ]
    -- Open: the floor runs through between lit rails, with arrows the way to go.
    threshold = Pictures $
      [ Color rail (wRect (x0 - 1) (y0 + 1) (x0 + 1.5) (y1 - 1))
      , Color rail (wRect (x1 - 1.5) (y0 + 1) (x1 + 1) (y1 - 1))
      ]
      ++ [ wAt (cx, cy + o) (Rotate pointing (Color (fade strength rail)
             (Pictures [plainSeg 2.5 (-3, 6) (3, 0), plainSeg 2.5 (3, 0) (-3, -6)])))
         | (o, i) <- [(-20, 0), (0, 1), (20, 2)]
         , let strength = if reduced then 1 else
                 0.7 + 0.3 * sin (fromIntegral clock * 2 * pi / 80 + i * 1.1 :: Float) ]
    rail = if open then doorColor door else gold
    pointing = case door of { DoorA -> 0; DoorB -> 180 } :: Float
    leaf ya yb s = Pictures
      [ Color iron (wRect x0 ya x1 yb)
      , Color ironHi (wRect (x0 + 2) ya (x0 + 4) yb)
      , Color (if open then tone else mix 0.5 tone iron) (wRect x0 (edgeY - 2) x1 (edgeY + 2))
      ]
      where edgeY = if s > (0 :: Float) then yb else ya
    -- Shut: diagonal hazard bars in the door's colour, and a lock plate at the middle.
    sealedFace = Pictures $
      [ Color iron (wRect x0 y0 x1 y1) ]
      ++ [ Color (mix 0.15 tone iron) (Polygon (map wp
             [ (x0, y), (x1, y - 8), (x1, y - 2), (x0, y + 6) ]))
         | y <- [y0 + 10, y0 + 22 .. y1 - 4] ]
      ++ [ Color ink (wRect x0 (cy - 12) x1 (cy + 12))
         , Color (doorColor door) (wRect (x0 + 1) (cy - 10.5) (x1 - 1) (cy + 10.5))
         , wAt (cx, cy) (Color ivory (Pictures (lockGlyph door)))
         ]
    -- The player is standing in a door that has run out: it is holding for them only.
    heldMark = case door of
      DoorA -> wAt (cx - 24, y0 - 8) (Color gold (textR 11 "HOLDING"))
      DoorB -> wAt (cx - 24, y1 + 34) (Color gold (textR 11 "HOLDING"))
    slam = Pictures
      [ Color (fade (0.55 * seal * seal) ivory) (wRect (x0 - 3) y0 (x1 + 3) y1)
      , Color (fade (0.2 * seal) ivory) (wRect (x0 - 12) y0 (x1 + 12) y1) ]

lockGlyph :: DoorId -> [Picture]
lockGlyph door = case door of
  DoorA -> [band 12 3 5]
  DoorB -> [diamond 5.5, Color (doorColor door) (diamond 2.8)]

-- | The courier from above: bowler hat, narrow shoulders, satchel behind, and a pocket
-- watch swinging on its chain. Faces along +x.
courier :: Bool -> Float -> Float -> Float -> Bool -> (Float, Float) -> Picture
courier reduced heading stride bob moving pos = wAt pos $ Pictures
  [ Translate 2.5 (-3.5) (Color (fade 0.28 ink) (ellipse 18 14 14))
  , Rotate (heading * 180 / pi) $ Pictures
      [ foot 1
      , foot (-1)
      , satchel
      , watch
      , Color ink (ellipse 20 9.5 14)
      , Color coat (ellipse 20 8 12.5)
      , Color (hex 0xE6C7A0) (ellipse 12 3 4 `shifted` (7.5, 0))
      , Color ink (ngon 20 9.8)
      , Color brim (ngon 20 8.8)
      , Color band' (ngon 18 6.6)
      , Color crown (ngon 18 5.6)
      , Color (fade 0.55 (hex 0xFFF1D6)) (arcBand 6 3.2 4.8 95 175)
      ]
  ]
  where
    coat = hex 0x1F2F52
    brim = hex 0x4B3423
    crown = hex 0x2C1D13
    band' = hex 0xC8A24A
    shifted p (x, y) = Translate x y p
    swing = if moving then sin (stride * 0.21) else 0
    foot s = Translate (s * swing * 7) (s * 7) (Pictures
      [ Color ink (ellipse 8 5.5 3.8), Color (hex 0x3A2A1C) (ellipse 8 4.5 2.9) ])
    satchel = Translate (-9.5) (-1) $ Rotate 0 $ Pictures
      [ Color ink (rectC 10 15)
      , Color (hex 0x7A2A1D) (rectC 8 13)
      , Color brassHi (rectC 2.5 3)
      ]
    watch =
      let a = if reduced then 0 else 0.5 * swing + 0.12 * bob
          wx = -13 * cos a
          wy = -13 * sin a + 10
      in Pictures
        [ Color brassLo (plainSeg 1 (-3, 9) (wx, wy))
        , at (wx, wy) (Pictures [Color brassLo (ngon 10 3.6), Color gold (ngon 10 2.6)])
        ]

-- | A short gilt trail, fading behind the courier.
trail :: [Vec] -> Picture
trail ps = Pictures
  [ wAt (f2 p) (Color (fade (0.42 * k) gold) (diamond (1 + 2.4 * k)))
  | (i, p) <- zip [0 :: Int ..] ps
  , i >= 3, even i
  , let k = 1 - fromIntegral i / fromIntegral maxTrail ]

particles :: [Particle] -> Picture
particles = Pictures . map one
  where
    one pt =
      let life = 1 - fromIntegral (ptAge pt) / fromIntegral (ptLife pt) :: Float
      in wAt (f2 (ptPos pt)) $ case ptKind pt of
        Spark door -> Color (fade (0.9 * life) (doorLight door)) (diamond (1.2 + 2 * life))
        Soot -> Color (fade (0.7 * life) (hex 0x3B3A36)) (rectC 2.4 2.4)
        Gilt -> Color (fade (min 1 (life * 1.6)) gold) (ngon 6 (1.5 + 2.2 * life))

-- | The way out: light and a slowly turning vault wheel.
exitLive :: Bool -> Picture -> Int -> (Float, Float) -> Picture
exitLive reduced spokes clock p = wAt p $ Pictures $
  [ glow gold (58 + 5 * breathe) 4 | not reduced ]
  ++ [ Rotate (fromIntegral clock * 0.4) spokes ]
  where
    breathe = sin (fromIntegral clock * 2 * pi / 200) :: Float
