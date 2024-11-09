-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Animation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Exit (exitSuccess)
import System.Random

-- | Handle one iteration
step :: Float -> GameState -> IO GameState
step secs g = return g

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyDelete) Down _ _) gstate = do exitSuccess
input (EventKey key Down _ _) g = return g
input _ g = return g  -- Catch-all pattern for other events