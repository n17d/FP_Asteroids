-- | This module defines how to turn
--   the game state into a picture
module View where

import Animation
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view GameState{player = p} = return $ scale 10 10 $ pic
         where pic = pictures(drawPlayer p)
