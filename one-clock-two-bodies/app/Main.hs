-- | The only part of the program that talks to the window. Gloss calls these
-- functions for us; the game itself stays pure.
module Main (main) where

import Data.Char (toLower)

import Graphics.Gloss.Interface.Pure.Game

import Game.Input
import Game.Level
import Game.Render
import Game.Types

main :: IO ()
main = play
  (InWindow "One Clock, Two Bodies" (960, 600) (100, 100))
  backgroundColor
  30                                -- frames per second
  (initialApp level1)
  (renderGame level1 . appGame)
  (handleEvent level1)
  (\_seconds app -> app)            -- nothing happens with the passing of real time

handleEvent :: Level -> Event -> App -> App
handleEvent level (EventKey key keyState _ _) app =
  case (keyToAction key, keyState) of
    (Just action, Down) -> pressAction level action app
    (Just action, Up)   -> releaseAction action app
    (Nothing, _)        -> app
handleEvent _ _ app = app

keyToAction :: Key -> Maybe Action
keyToAction (SpecialKey KeyUp)    = Just (Move DirUp)
keyToAction (SpecialKey KeyDown)  = Just (Move DirDown)
keyToAction (SpecialKey KeyLeft)  = Just (Move DirLeft)
keyToAction (SpecialKey KeyRight) = Just (Move DirRight)
keyToAction (SpecialKey KeyTab)   = Just SwitchTime
-- toLower so Caps Lock, or Shift released mid-press, cannot leave a key stuck.
keyToAction (Char c) = case toLower c of
  'w' -> Just (Move DirUp)
  's' -> Just (Move DirDown)
  'a' -> Just (Move DirLeft)
  'd' -> Just (Move DirRight)
  'r' -> Just Restart
  _   -> Nothing
keyToAction _ = Nothing
