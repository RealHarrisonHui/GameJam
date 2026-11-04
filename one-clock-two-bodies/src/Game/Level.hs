-- | Levels are drawn as text, so changing a puzzle means editing a picture.
module Game.Level
  ( level1
  , parseLevel
  ) where

import Game.Types

-- | The proof-of-mechanic level.
--
-- > #  wall        .  floor
-- > A  A's spawn   a  A's goal
-- > B  B's spawn   b  B's goal
-- > P  plate P     1  door 1 (opened by P)
-- > Q  plate Q     2  door 2 (opened by Q)
level1 :: Level
level1 = parseLevel
  [ "###############"
  , "###############"
  , "#A.P.....2..a.#"
  , "###############"
  , "#B...1...Q..b.#"
  , "###############"
  , "###############"
  ]

-- | Turn a text map into a 'Level'. A malformed map stops the program with a
-- message saying what is wrong, because that is a mistake in the source code.
parseLevel :: [String] -> Level
parseLevel rows
  | any ((/= width) . length) rows = error "Level rows must all be the same length"
  | any (`notElem` "#.ABPQ12ab") (concat rows) = error "Level contains an unknown character"
  | otherwise = Level
      { levelWidth  = width
      , levelHeight = length rows
      , levelWalls  = tilesOf '#'
      , levelPlates = [(PlateP, p) | p <- tilesOf 'P'] ++ [(PlateQ, p) | p <- tilesOf 'Q']
      , levelDoors  = [(Door1, p) | p <- tilesOf '1'] ++ [(Door2, p) | p <- tilesOf '2']
      , levelSpawnA = exactlyOne 'A'
      , levelSpawnB = exactlyOne 'B'
      , levelGoalA  = exactlyOne 'a'
      , levelGoalB  = exactlyOne 'b'
      }
  where
    width = maximum (0 : map length rows)

    tilesOf c =
      [ (x, y)
      | (y, row) <- zip [0 ..] rows
      , (x, tile) <- zip [0 ..] row
      , tile == c
      ]

    exactlyOne c = case tilesOf c of
      [p] -> p
      _   -> error ("Level must contain exactly one '" ++ [c] ++ "'")
