-- | This module defines how to turn
--   the game state into a picture
module View where

import Animation
import Graphics.Gloss
import Model

view :: GameState -> IO Picture