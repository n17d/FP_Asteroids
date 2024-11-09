-- | This module defines how to turn
--   the game state into a picture
module View where

import Animation
import Graphics.Gloss
import Model
import Enemy
import Bullet

view :: GameState -> IO Picture
view GameState{player = p@Player{rotation = rot, direction = dir}, bullets = bs, time = steps, state = st, enemies = e}
        | st == Pause = return $ scale 10 10 $ pictures ([color yellow $ translate (-35) 0 $ scale 0.05 0.05 $ text "You paused the game."] ++ [color yellow $ translate (-53) (-10) $ scale 0.05 0.05 $ text "Click P or ESC again to unpause."])
        | otherwise = return $ scale 10 10 $ pic
          where pic = pictures(drawPlayer p ++ rotation ++ directionDegrees ++ concatMap drawBullet bs ++ showTime ++ concatMap drawEnemy e)
                rotation = [color yellow $ translate 70 10 $ scale 0.01 0.01 $ text ("rotation: " ++ show (rot))]
                directionDegrees = [color yellow $ translate 70 20 $ scale 0.01 0.01 $ text ("direction: " ++ show (dir))]
                showTime = [color yellow $ translate 70 0 $ scale 0.01 0.01 $ text ("Time: " ++ show (floor (steps / 30)))]
