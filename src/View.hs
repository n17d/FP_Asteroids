-- | This module defines how to turn
--   the game state into a picture
module View where

import Animation
import Graphics.Gloss
import Model
import Enemy
import Bullet
import Asteroid

view :: GameState -> IO Picture
view GameState{player = p@Player{rotation = rot, direction = dir, lives = hp}, bullets = bs, time = steps, state = st, enemies = e, asteroids = as}
        | st == GameOver = return $ scale 10 10 $ pictures ([color white $ translate 0 0 $ scale 0.01 0.01 $ text "GAMEOVER"] ++ [color white $ translate (-5) (-10) $ scale 0.01 0.01 $ text "Press G to continue"])
        | st == Pause = return $ scale 10 10 $ pictures ([color yellow $ translate (-35) 0 $ scale 0.05 0.05 $ text "You paused the game."] ++ [color yellow $ translate (-53) (-10) $ scale 0.05 0.05 $ text "Click P or ESC again to unpause."])
        | otherwise = return $ scale 10 10 $ pic
          where pic = pictures(drawPlayer p ++ concatMap drawBullet bs ++ showTime ++ concatMap drawEnemy e ++ concatMap drawAsteroid as ++ showLives)
                showTime = [color yellow $ translate 70 0 $ scale 0.01 0.01 $ text ("TIME: " ++ show (floor (steps / 30)))]
                showLives = [color yellow $ translate 70 40 $ scale 0.01 0.01 $ text ("LIVES: " ++ show (hp))]
                scoreAmount = [color yellow $ translate 70 50 $ scale 0.01 0.01 $ text ("SCORE: " ++ show (hp))]
