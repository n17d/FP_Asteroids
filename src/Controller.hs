-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Animation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Maze
import Model
import System.Exit (exitSuccess)
import System.Random

-- | Handle one iteration
step :: Float -> GameState -> IO GameState

-- | Handle user input
input :: Event -> GameState -> IO GameState