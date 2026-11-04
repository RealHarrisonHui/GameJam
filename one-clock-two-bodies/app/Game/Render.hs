-- | Draws a game state. It reads the state and returns a picture; it never
-- changes anything. Gloss puts (0, 0) at the window centre with y going up.
module Game.Render
  ( renderGame
  , backgroundColor
  ) where

import Graphics.Gloss

import Game.Logic
import Game.Types

renderGame :: Level -> Game -> Picture
renderGame level game = pictures
  [ drawMap level
  , pictures (map (drawGoal level) [CharA, CharB])
  , pictures (map (drawPlate level game) (levelPlates level))
  , pictures (map (drawDoor level game) (levelDoors level))
  , drawCharacter game CharA
  , drawCharacter game CharB
  , drawHud game
  , if phase game == Won then drawVictory else blank
  ]

-- Palette. Each character and each plate/door pair has one colour, so the
-- matching colour is what links a plate to the door it opens.

backgroundColor, floorColor, wallColor, inkColor, darkInk, activeRing :: Color
backgroundColor = greyN 0.10
floorColor      = greyN 0.30
wallColor       = greyN 0.15
inkColor        = greyN 0.88
darkInk         = greyN 0.05
activeRing      = makeColorI 255 240 90 255

characterColor :: Character -> Color
characterColor CharA = makeColorI 240 150 60 255
characterColor CharB = makeColorI 90 175 240 255

plateColor :: PlateId -> Color
plateColor PlateP = makeColorI 70 205 150 255
plateColor PlateQ = makeColorI 215 110 215 255

-- Layout. The map sits between the HUD at the top and the help at the bottom.

tileSize :: Float
tileSize = 56

mapLeft, mapTop :: Float
mapLeft = -420
mapTop  = 220

-- | Centre of a tile, in window coordinates.
tileCentre :: Pos -> (Float, Float)
tileCentre (x, y) =
  ( mapLeft + tileSize * (fromIntegral x + 0.5)
  , mapTop - tileSize * (fromIntegral y + 0.5)
  )

atTile :: Pos -> Picture -> Picture
atTile p = uncurry translate (tileCentre p)

-- Map and objects.

drawMap :: Level -> Picture
drawMap level = pictures
  [ atTile (x, y) (drawTile (x, y))
  | x <- [0 .. levelWidth level - 1]
  , y <- [0 .. levelHeight level - 1]
  ]
  where
    drawTile p
      | p `elem` levelWalls level = color wallColor (rectangleSolid tileSize tileSize)
      | otherwise = pictures
          [ color floorColor (rectangleSolid tileSize tileSize)
          , color wallColor (rectangleWire tileSize tileSize)
          ]

drawGoal :: Level -> Character -> Picture
drawGoal level who = pictures
  [ atTile p (color (characterColor who) (thickSquare 44 4))
  , atTile p (centredText 0.16 (-6) (characterColor who) goalLetter)
  , note p above [goalName]
  ]
  where
    (p, above, goalLetter, goalName) = case who of
      CharA -> (levelGoalA level, True, "a", "A's goal")
      CharB -> (levelGoalB level, False, "b", "B's goal")

drawPlate :: Level -> Game -> (PlateId, Pos) -> Picture
drawPlate level game (plate, p) = pictures
  [ atTile p (color col shape)
  , atTile p (centredText 0.16 (-6) letterColor letter)
  , note p (plate == PlateP)
      ["plate " ++ letter ++ ": " ++ (if held then "HELD" else "empty"), "opens door " ++ door]
  ]
  where
    col   = plateColor plate
    held  = plateOccupied level game plate
    shape = if held then rectangleSolid 46 46 else thickSquare 40 3
    letterColor = if held then darkInk else col
    (letter, door) = case plate of
      PlateP -> ("P", "1")
      PlateQ -> ("Q", "2")

drawDoor :: Level -> Game -> (DoorId, Pos) -> Picture
drawDoor level game (door, p)
  | open = pictures
      [ atTile p (color col (thickSquare 50 2))
      , atTile p (centredText 0.14 (-5) col number)
      , label "OPEN"
      ]
  | otherwise = pictures
      [ atTile p (color col (rectangleSolid tileSize tileSize))
      , atTile p (color darkInk (thickSquare 44 3))
      , atTile p (centredText 0.22 (-8) darkInk number)
      , label "CLOSED"
      ]
  where
    open   = doorOpen level game door
    col    = plateColor (linkedPlate door)
    number = case door of
      Door1 -> "1"
      Door2 -> "2"
    label status = note p (door == Door2) ["door " ++ number ++ ": " ++ status]

drawCharacter :: Game -> Character -> Picture
drawCharacter game who = atTile (positionOf who game) (pictures
  [ if active then color activeRing (thickCircle 24 5) else blank
  , color bodyColor (circleSolid 19)
  , centredText 0.18 (-7) darkInk letter
  ])
  where
    active    = activeChar game == who
    bodyColor = if active then characterColor who else frozenTint (characterColor who)
    letter    = characterLetter who

-- Text around the map.

drawHud :: Game -> Picture
drawHud game = pictures
  [ leftText (-420) 240 0.30 (characterColor who) ("TIME: " ++ characterLetter who)
  , leftText (-180) 250 0.13 inkColor (characterLetter who ++ " can move.")
  , leftText (-180) 228 0.13 inkColor (characterLetter (other who) ++ " is frozen, but still holds plates.")
  , leftText (-420) (-205) 0.14 inkColor "Objective: get A onto goal a and B onto goal b."
  , leftText (-420) (-240) 0.14 inkColor "Move: WASD or arrow keys     Tab: transfer time     R: restart"
  ]
  where
    who = activeChar game
    other CharA = CharB
    other CharB = CharA

drawVictory :: Picture
drawVictory = pictures
  [ color (withAlpha 0.85 darkInk) (rectangleSolid 450 150)
  , color activeRing (rectangleWire 450 150)
  , centredText 0.38 10 activeRing "Both arrived!"
  , centredText 0.20 (-45) inkColor "Press R to restart"
  ]

-- | Small caption on the wall directly above or below a tile.
note :: Pos -> Bool -> [String] -> Picture
note p above textLines = atTile p (pictures
  [ centredText 0.105 (firstY - fromIntegral i * step) inkColor str
  | (i, str) <- zip [0 :: Int ..] textLines
  ])
  where
    step = 15
    lineCount = fromIntegral (length textLines)
    firstY
      | above     = 34 + (lineCount - 1) * step
      | otherwise = -44

-- Small drawing helpers.

characterLetter :: Character -> String
characterLetter CharA = "A"
characterLetter CharB = "B"

frozenTint :: Color -> Color
frozenTint = mixColors 0.55 0.45 (greyN 0.25)

-- | A square outline with a visible thickness.
thickSquare :: Float -> Float -> Picture
thickSquare size thickness = pictures
  [ translate 0 offset horizontal
  , translate 0 (-offset) horizontal
  , translate offset 0 vertical
  , translate (-offset) 0 vertical
  ]
  where
    offset     = (size - thickness) / 2
    horizontal = rectangleSolid size thickness
    vertical   = rectangleSolid thickness size

leftText :: Float -> Float -> Float -> Color -> String -> Picture
leftText x y size col str =
  translate x y (color col (boldText size str))

-- | Text centred horizontally on x = 0.
centredText :: Float -> Float -> Color -> String -> Picture
centredText size y col str =
  leftText (negate (estimatedWidth / 2)) y size col str
  where
    -- Gloss cannot measure text, so this uses the font's typical letter width.
    estimatedWidth = fromIntegral (length str) * 68 * size

-- | Gloss text is a thin line drawing. Drawing it twice, slightly offset, makes
-- it easier to read.
boldText :: Float -> String -> Picture
boldText size str = pictures
  [ scale size size (text str)
  , translate (5 * size) 0 (scale size size (text str))
  ]
