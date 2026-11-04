-- | "The Stopped Hour": every image in the scene, painted by pure functions.
module Art.Scene
  ( Assets
  , Item (..)
  , makeAssets
  , allSprites
  , drawWorld
  , sceneWidth
  , sceneHeight
  , tileX
  , rowY
  , slabLeft
  , slabRight
  ) where

import Game.Types

import Art.Raster
import Art.Visual

sceneWidth, sceneHeight :: Int
sceneWidth = 960
sceneHeight = 600

-- Layout, in pixels from the top-left of the scene.

tileX :: Float -> Float
tileX tile = 103 + (tile - 0.5) * 58

slabTopA, slabTopB, slabLeft, slabRight :: Float
slabTopA = 276
slabTopB = 442
slabLeft = 103
slabRight = 857

rowY :: Character -> Float
rowY CharA = slabTopA
rowY CharB = slabTopB

-- | The underside of a slab bulges slightly in the middle, like a hanging stone.
slabBottom :: Float -> Float -> Float
slabBottom top x =
  let u = (x - (slabLeft + slabRight) / 2) / ((slabRight - slabLeft) / 2)
  in top + 9 + 15 * (1 - u * u)

moonX, moonY, moonR :: Float
moonX = 480
moonY = 232
moonR = 196

doorHeight :: Float
doorHeight = 104

-- Palette.

amber, sky, jade, orchid, stone, paleWarm, rose, ink :: C3
amber = hex 0xffb04f
sky = hex 0x66c6ff
jade = hex 0x4fe8b4
orchid = hex 0xe68aff
stone = hex 0x6f6a84
paleWarm = hex 0xffe2c4
rose = hex 0xff9e8a
ink = hex 0x120e24

characterColor :: Character -> C3
characterColor CharA = amber
characterColor CharB = sky

-- | The images that make up the scene.
data Assets = Assets
  { assetSky     :: Sprite
  , assetFront   :: Sprite
  , assetDawn    :: [Sprite]
  , assetHands   :: [Sprite]
  , assetWireP   :: [Sprite]
  , assetWireQ   :: [Sprite]
  , assetDoor1   :: [Sprite]
  , assetDoor2   :: [Sprite]
  , assetPlateP  :: [Sprite]
  , assetPlateQ  :: [Sprite]
  , assetBodyA   :: [Sprite]
  , assetBodyB   :: [Sprite]
  , assetPoolA   :: [Sprite]
  , assetPoolB   :: [Sprite]
  , assetSpark   :: Sprite
  , assetOrbs    :: [Sprite]
  }

allSprites :: Assets -> [Sprite]
allSprites a =
  [assetSky a, assetFront a, assetSpark a]
    ++ concatMap ($ a)
         [ assetDawn, assetHands, assetWireP, assetWireQ, assetDoor1, assetDoor2
         , assetPlateP, assetPlateQ, assetBodyA, assetBodyB, assetPoolA, assetPoolB
         , assetOrbs ]

-- | Evenly spaced values from 0 to 1.
steps :: Int -> [Float]
steps n = [fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1]]

lifeLevels, squashLevels, doorLevels, glowLevels :: Int
lifeLevels = 8
squashLevels = 9
doorLevels = 13
glowLevels = 7

makeAssets :: Assets
makeAssets = Assets
  { assetSky = bake (0, 0, sceneWidth, sceneHeight) skyShader
  , assetFront = bake (0, 0, sceneWidth, sceneHeight) frontShader
  , assetDawn = [bake moonBox (dawnShader v) | v <- steps glowLevels]
  , assetHands = [handSprite i | i <- [0 .. 59]]
  , assetWireP = [wireSprite jade wireP v | v <- steps glowLevels]
  , assetWireQ = [wireSprite orchid wireQ v | v <- steps glowLevels]
  , assetDoor1 = [bake doorBox (doorShader jade o) | o <- steps doorLevels]
  , assetDoor2 = [bake doorBox (doorShader orchid o) | o <- steps doorLevels]
  , assetPlateP = [bake plateBox (plateShader jade v) | v <- steps glowLevels]
  , assetPlateQ = [bake plateBox (plateShader orchid v) | v <- steps glowLevels]
  , assetBodyA = bodyFrames CharA
  , assetBodyB = bodyFrames CharB
  , assetPoolA = [bake poolBox (poolShader amber v) | v <- steps lifeLevels]
  , assetPoolB = [bake poolBox (poolShader sky v) | v <- steps lifeLevels]
  , assetSpark = bake (-8, -8, 16, 16) (lightShader paleWarm 1.6 4)
  , assetOrbs = [bake (-46, -46, 92, 92) (lightShader paleWarm (5 * s) (17 * s)) | s <- [0.28, 0.42, 0.6, 0.8, 1]]
  }
  where
    moonBox = (round moonX - 300, round moonY - 300, 600, 600)
    doorBox = (-52, -142, 104, 156)
    plateBox = (-46, -60, 92, 68)
    poolBox = (-64, -12, 128, 24)

-- The far background.

skyShader :: Shader
skyShader x y = layers
  [ tint (skyGradient (y / 600)) 1
  , if dist > moonR then stars x y else clear
  , tint rose (0.42 * exp (negate dist / 170))
  , tint rose (discEdge * (0.04 + 0.2 * clamp01 ((moonR - dist) / moonR)))
  , tint paleWarm (0.46 * hairline (dist - moonR) 0.55)
  , tint paleWarm (0.5 * ticks)
  , ripple (moonR + 34) 0.18
  , ripple 310 0.1
  , ripple 395 0.075
  , ripple 490 0.055
  , hill 512 22 0.009 1.3 13 0.023 0.4 (hex 0x47204f)
  , hill 548 16 0.013 4.1 7 0.031 2.2 (hex 0x25132f)
  , tint ink (0.46 * vignette)
  , grain
  ]
  where
    dist = sqrt ((x - moonX) * (x - moonX) + (y - moonY) * (y - moonY))
    discEdge = cover (dist - moonR)
    ripple r a = tint paleWarm (a * hairline (dist - r) 0.5)
    angle = atan2 (y - moonY) (x - moonX)
    tickStep = 2 * pi / 60 :: Float
    nearest = fromIntegral (round (angle / tickStep) :: Int) * tickStep
    index = round (angle / tickStep) :: Int
    major = index `mod` 5 == 0
    tickInner = moonR + (if major then 8 else 13)
    across = dist * abs (sin (angle - nearest))
    alongTick = max (tickInner - dist) (dist - (moonR + 22))
    ticks = cover (max (across - (if major then 0.9 else 0.45)) alongTick)
    hill base a1 f1 p1 a2 f2 p2 colour =
      let top = base + a1 * sin (x * f1 + p1) + a2 * sin (x * f2 + p2)
      in tint colour (0.9 * smoothstep (-2) 3 (y - top))
    vx = (x - 480) / 480
    vy = (y - 300) / 300
    vignette = smoothstep 0.55 1.45 (sqrt (vx * vx + vy * vy))
    grain = let n = hash2 (floor x * 3 + 17) (floor y * 7 + 5)
            in if n > 0.5 then tint paleWarm ((n - 0.5) * 0.045) else tint ink ((0.5 - n) * 0.09)

skyGradient :: Float -> C3
skyGradient t
  | t < 0.42  = mix3 (t / 0.42) (hex 0x0f0c2a) (hex 0x2c1c55)
  | t < 0.72  = mix3 ((t - 0.42) / 0.3) (hex 0x2c1c55) (hex 0x7a2f6a)
  | t < 0.9   = mix3 ((t - 0.72) / 0.18) (hex 0x7a2f6a) (hex 0xd35f6c)
  | otherwise = mix3 ((t - 0.9) / 0.1) (hex 0xd35f6c) (hex 0xf29a6e)

-- | A scatter of small stars, stronger towards the top of the sky.
stars :: Shader
stars x y =
  let cell = 22 :: Float
      cx = floor (x / cell) :: Int
      cy = floor (y / cell) :: Int
      exists = hash2 cx cy > 0.72
      px = (fromIntegral cx + 0.15 + 0.7 * hash2 (cx + 91) cy) * cell
      py = (fromIntegral cy + 0.15 + 0.7 * hash2 cx (cy + 37)) * cell
      r = 0.45 + 0.75 * hash2 (cx + 7) (cy + 11)
      d = sdCircle px py r x y
      fade = 1 - smoothstep 90 390 y
      bright = 0.25 + 0.75 * hash2 (cx + 13) (cy + 3)
  in if exists then tint paleWarm (cover d * fade * bright) else clear

-- | Falls smoothly to zero at a distance, so that glows never show the square
-- edge of the image they were painted into.
fadeOut :: Float -> Float -> Float
fadeOut reach d = 1 - smoothstep 0 reach d
{-# INLINE fadeOut #-}

hairline :: Float -> Float -> Float
hairline d width = cover (abs d - width)
{-# INLINE hairline #-}

-- | The clock face floods with light once both bodies have arrived.
dawnShader :: Float -> Shader
dawnShader v x y =
  let dist = sqrt ((x - moonX) * (x - moonX) + (y - moonY) * (y - moonY))
      disc = cover (dist - moonR) * (0.2 + 0.25 * clamp01 (1 - dist / moonR))
      outside = max 0 (dist - moonR)
      halo = 0.42 * exp (negate outside / 60) * fadeOut 100 outside
  in tint paleWarm (v * (disc + halo * (1 - cover (dist - moonR))))

-- | The clock hand, which steps once for every move. Time passes only when a
-- body uses it.
handSprite :: Int -> Sprite
handSprite i = bake box shader
  where
    angle = fromIntegral i * 2 * pi / 60 - pi / 2 :: Float
    tipX = moonX + 176 * cos angle
    tipY = moonY + 176 * sin angle
    tailX = moonX - 30 * cos angle
    tailY = moonY - 30 * sin angle
    left = floor (minimum [tipX, tailX, moonX]) - 16
    top = floor (minimum [tipY, tailY, moonY]) - 16
    right = ceiling (maximum [tipX, tailX, moonX]) + 16
    bottom = ceiling (maximum [tipY, tailY, moonY]) + 16
    box = (left, top, right - left, bottom - top)
    shader x y =
      let d = sdSegment tailX tailY tipX tipY x y
          hub = sdCircle moonX moonY 4.5 x y
      in layers
           [ tint paleWarm (0.26 * exp (negate d / 5))
           , tint paleWarm (0.95 * cover (d - 0.9))
           , tint paleWarm (cover hub)
           , tint ink (cover (sdCircle moonX moonY 1.6 x y))
           ]

-- The stage in front of the clock: stone, plate sockets, door posts, wiring.

frontShader :: Shader
frontShader x y = layers (concat
  [ slab slabTopA x y
  , slab slabTopB x y
  , wireAt jade wireP 0.2
  , wireAt orchid wireQ 0.2
  , socket jade 3 slabTopA
  , socket orchid 9 slabTopB
  , posts jade 5 slabTopB
  , posts orchid 9 slabTopA
  , goal CharA 12
  , goal CharB 12
  ])
  where
    wireAt colour wire a =
      [tint colour (a * cover (wireDistance wire x y - 0.5))]
    socket colour tile top =
      [tint colour (0.5 * hairline (sdBox (tileX tile) (top - 2) 20 2.5 2.5 x y) 0.45)]
    posts colour tile top =
      let dx = x - tileX tile
          post side = sdBox side (top - doorHeight / 2) 0.8 (doorHeight / 2) 0.8 dx y
          lintel = sdBox 0 (top - doorHeight) 16 0.8 0.8 dx y
      in [tint colour (0.55 * cover (minimum [post (-15), post 15, lintel]))]
    goal who tile =
      let colour = characterColor who
          d = bodyDistance who 0 (x - tileX tile) (y - rowY who)
      in [ tint colour (0.75 * hairline d 0.5)
         , tint colour (0.1 * cover d)
         ]

-- | One floating stone: a soft shadow below it, a dark body and a lit top edge.
slab :: Float -> Shader'
slab top x y
  | y < top - 6 || y > top + 112 = []
  | otherwise =
      [ tint ink (0.4 * shadow)
      , tint body (cover edge)
      , tint rimColour (rim * cover (max (abs (y - top) - 0.9) sideEdge))
      ]
  where
    bottom = slabBottom top x
    sideEdge = max (slabLeft - x) (x - slabRight)
    edge = maximum [top - y, y - bottom, sideEdge]
    below = y - bottom
    across = clamp01 (1 - abs (x - 480) / 400)
    shadow = if below > 0 then exp (negate below / 26) * smoothstep 0 0.4 across else 0
    depth = clamp01 ((y - top) / (bottom - top))
    body = mix3 depth (hex 0x2c2342) (hex 0x130e22)
    toMoon = sqrt ((x - moonX) * (x - moonX) + (top - moonY) * (top - moonY))
    rim = 0.35 + 0.65 * exp (negate toMoon / 260)
    rimColour = hex 0xffcfa8

type Shader' = Float -> Float -> [RGBA]

-- The lines of light that link a plate to its door.

type Wire = [(Float, Float, Float, Float)]

wireP, wireQ :: Wire
wireP =
  [ (tileX 3, slabBottom slabTopA (tileX 3) - 2, tileX 3, slabTopA + 40)
  , (tileX 3, slabTopA + 40, tileX 5, slabTopA + 40)
  , (tileX 5, slabTopA + 40, tileX 5, slabTopB - doorHeight)
  ]
wireQ =
  [ (tileX 9, slabTopB - 3, tileX 9, slabBottom slabTopA (tileX 9) - 2)
  ]

wireDistance :: Wire -> Float -> Float -> Float
wireDistance wire x y = minimum [sdSegment a b c d x y | (a, b, c, d) <- wire]

wireSprite :: C3 -> Wire -> Float -> Sprite
wireSprite colour wire v = bake (left, top, right - left, bottom - top) shader
  where
    xs = concat [[a, c] | (a, _, c, _) <- wire]
    ys = concat [[b, d] | (_, b, _, d) <- wire]
    left = floor (minimum xs) - 18
    right = ceiling (maximum xs) + 18
    top = floor (minimum ys) - 18
    bottom = ceiling (maximum ys) + 18
    shader x y =
      let d = wireDistance wire x y
      in layers
           [ tint colour (v * 0.4 * exp (negate d / 5))
           , tint colour (v * 0.95 * cover (d - 0.8))
           ]

-- Doors and plates. Their shaders use a position relative to the base of the
-- door or the centre of the plate.

-- | Two curtains of light that slide apart as the door opens.
doorShader :: C3 -> Float -> Shader
doorShader colour open x y = layers
  [ tint colour (0.34 * closed * exp (negate outside / 11) * fadeOut 34 outside)
  , tint colour (fill * inDoor)
  , tint colour (edgeLight * inDoor)
  , tint colour ((0.45 + 0.55 * closed) * cover (min (min (post (-15)) (post 15)) lintel))
  ]
  where
    closed = 1 - open
    box = sdBox 0 (negate doorHeight / 2) 15 (doorHeight / 2) 1 x y
    outside = max 0 box
    inDoor = cover box
    gap = 15 * open
    curtain = cover (abs x - gap)
    height = clamp01 (negate y / doorHeight)
    towardsSeam = 1 - clamp01 ((abs x - gap) / 15)
    fill = (1 - curtain) * (0.1 + 0.34 * towardsSeam * towardsSeam + 0.12 * height)
    edgeLight = if open > 0.98 then 0 else 0.9 * cover (abs (abs x - gap) - 0.6)
    post side = sdBox side (negate doorHeight / 2) 1 (doorHeight / 2) 1 x y
    lintel = sdBox 0 (negate doorHeight) 16 1 1 x y

-- | A pressed plate shines upward.
plateShader :: C3 -> Float -> Shader
plateShader colour v x y = layers
  [ tint colour (v * 0.5 * shaft)
  , tint colour (v * 0.5 * exp (negate (max 0 pill) / 7))
  , tint colour ((0.1 + 0.9 * v) * cover pill)
  ]
  where
    pill = sdBox 0 (-2) 20 2.5 2.5 x y
    above = max 0 (negate y)
    shaft = if y < 0 then exp (negate above / 20) * (1 - smoothstep 12 30 (abs x)) else 0

-- | A pool of colour on the stone under a living body.
poolShader :: C3 -> Float -> Shader
poolShader colour v x y =
  tint colour (v * 0.7 * exp (negate (x * x / 700 + y * y / 9)))

-- | A soft ball of light: a bright core and a wide glow.
lightShader :: C3 -> Float -> Float -> Shader
lightShader colour core glow x y =
  let d = sqrt (x * x + y * y)
      halo = max 0 (1 - d / (glow * 2.6))
  in layers
       [ tint colour (0.55 * halo * halo * exp (negate d / glow))
       , tint colour (cover (d - core))
       ]

-- The characters. A is a tall, narrow pillar. B is low and round, with a ring
-- around its head. Neither has a face: posture and light carry everything.

-- | Signed distance to a character, with its feet at (0, 0).
bodyDistance :: Character -> Float -> Float -> Float -> Float
bodyDistance who squash x y = min bodyPart headPart * min sx sy
  where
    sy = 1 + squash
    sx = 1 - 0.55 * squash
    ux = x / sx
    uy = y / sy
    bodyPart = case who of
      CharA ->
        let taper = 1 + 0.42 * clamp01 (negate uy / 62)
        in sdBox 0 (-31) 12.5 31 12.5 (ux * taper) uy
      -- A dome: a circle with its base cut flat.
      CharB -> max (sdCircle 0 (-14) 25 ux uy) (uy + 0.5)
    headPart = uncurry (\hx hy -> sdCircle hx hy (headRadius who) ux uy) (headAt who)

-- | Where a character's head sits, relative to its feet.
headAt :: Character -> (Float, Float)
headAt CharA = (0, -77)
headAt CharB = (0, -57)

headRadius :: Character -> Float
headRadius CharA = 8.5
headRadius CharB = 9.5

-- | The ring that a spark travels around while a body has time.
orbitRadius :: Character -> Float
orbitRadius CharA = 15
orbitRadius CharB = 17

bodyFrames :: Character -> [Sprite]
bodyFrames who =
  [ bake (-74, -146, 148, 172) (bodyShader who life (squashValue j))
  | life <- steps lifeLevels
  , j <- [0 .. squashLevels - 1]
  ]

squashValue :: Int -> Float
squashValue j = (fromIntegral j - 4) * 0.03

bodyShader :: Character -> Float -> Float -> Shader
bodyShader who life squash x y = layers
  [ tint colour (life * 0.26 * exp (negate outside / 20) * fadeOut 52 outside)
  , tint colour (life * 0.3 * exp (negate outside / 5) * fadeOut 52 outside)
  , tint skin (cover d)
  , tint highlight ((0.12 + 0.26 * life) * rim * cover d)
  , ring
  ]
  where
    colour = characterColor who
    d = bodyDistance who squash x y
    outside = max 0 d
    height = clamp01 (negate y / 90)
    aliveSkin = mix3 height (mix3 0.22 colour ink) (mix3 0.35 colour (hex 0xffffff))
    frozenSkin = mix3 height (mix3 0.5 stone ink) (mix3 0.25 stone (hex 0xd8d4e8))
    skin = mix3 life frozenSkin aliveSkin
    highlight = mix3 life (hex 0xb9b4cc) (mix3 0.6 colour (hex 0xffffff))
    rim = smoothstep (-2.4) (-0.2) d
    (hx, hy) = headAt who
    sy = 1 + squash
    headDist = sdCircle hx (hy * sy) (orbitRadius who) x y
    ring = tint (mix3 life (hex 0xa29dba) colour) ((0.28 + 0.52 * life) * hairline headDist 0.45)

-- Drawing one frame.

-- | One thing to draw, in scene pixels.
data Item
  = Blit !Sprite !Int !Int
  | Fade !Float

-- | The frame for a moment of the world, painted from back to front.
drawWorld :: Assets -> World -> [Item]
drawWorld a world = concat
  [ [Blit (assetSky a) 0 0]
  , glowItem (assetDawn a) (visWin vis) 0 0
  , [Blit (assetHands a !! (visTick vis `mod` 60)) 0 0]
  , [Blit (assetFront a) 0 0]
  , glowItem (assetWireP a) (visPlateP vis) 0 0
  , glowItem (assetWireQ a) (visPlateQ vis) 0 0
  , [ Blit (pick (assetDoor1 a) (visDoor1 vis)) (round (tileX 5)) (round slabTopB)
    , Blit (pick (assetDoor2 a) (visDoor2 vis)) (round (tileX 9)) (round slabTopA)
    , Blit (pick (assetPlateP a) (visPlateP vis)) (round (tileX 3)) (round slabTopA)
    , Blit (pick (assetPlateQ a) (visPlateQ vis)) (round (tileX 9)) (round slabTopB)
    ]
  , character CharA (assetPoolA a) (assetBodyA a)
  , character CharB (assetPoolB a) (assetBodyB a)
  , orbTrail
  , [Fade (worldFade world) | worldFade world > 0]
  ]
  where
    vis = worldVisual world
    time = visTime vis

    character who pools frames =
      let body = bodyOf who vis
          ax = round (tileX (bodyTileX body)) :: Int
          ay = round (rowY who - bodyLift body) :: Int
          ground = round (rowY who) :: Int
          life = bodyLife body
          lifeIndex = round (life * fromIntegral (lifeLevels - 1)) :: Int
          squashIndex = clampIndex (round (bodySquash time body / 0.03) + 4)
          frame = frames !! (lifeIndex * squashLevels + squashIndex)
          (hx, hy) = headAt who
          turn = time * 2.4
          orbit = orbitRadius who
          sx = ax + round (hx + orbit * cos turn)
          sy = ay + round (hy + orbit * sin turn)
      in [ Blit (pick pools life) ax ground
         , Blit frame ax ay
         ] ++ [Blit (assetSpark a) sx sy | life > 0.45]

    clampIndex = max 0 . min (squashLevels - 1)

    orbTrail
      | visOrb vis > 1 = []
      | otherwise =
          [ Blit sprite (round ox) (round oy)
          | (sprite, delay) <- zip (assetOrbs a) [0.2, 0.15, 0.1, 0.05, 0]
          , let p = clamp01 (visOrb vis - delay)
          , let (ox, oy) = orbPosition p
          ]

    orbPosition p =
      let to = visOrbTo vis
          from = if to == CharA then CharB else CharA
          headPos who =
            let (hx, hy) = headAt who
            in (tileX (bodyTileX (bodyOf who vis)) + hx, rowY who + hy)
          (x0, y0) = headPos from
          (x1, y1) = headPos to
          e = p * p * (3 - 2 * p)
          arc = 58 * sin (pi * e)
      in (mixF e x0 x1 - arc, mixF e y0 y1 - arc * 0.35)

-- | Choose the frame nearest to a value between 0 and 1.
pick :: [Sprite] -> Float -> Sprite
pick frames v = frames !! round (clamp01 v * fromIntegral (length frames - 1))

glowItem :: [Sprite] -> Float -> Int -> Int -> [Item]
glowItem frames v x y = [Blit (pick frames v) x y | v > 0.01]
