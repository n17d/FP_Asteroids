-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Animation
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Exit (exitSuccess)
import System.Random
import Helper
import Bullet
import Enemy
import Asteroid

-- | Handle one iteration
step :: Float -> GameState -> IO GameState
step secs g@GameState{player = p@Player{position = (px, py), direction = dir}, bullets = bs, time = steps, state = st, enemies = e, asteroids = as, randomize = rand} | st == Play = return g{player = enemyPlayerCollision (bulletPlayerCollision (asteroidPlayerCollision (updatePlayer p) as) bs) e, bullets = (concatMap (enemyShoot steps) e) ++ (enemyBulletCollision (asteroidBulletCollision (concatMap updateBullet bs) as) e), time = steps + 1, enemies = (bulletEnemyCollision bs (concatMap (updateEnemy (px, py) dir) e)) ++ (spawnEnemy steps rand), asteroids = bulletAsteroidCollision bs ((spawnAsteroid steps rand (px, py)) ++ (concatMap updateAsteroid as)), randomize = mkStdGen $ round(steps * steps)}
                                                                                                                         | st == Pause = return g

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyDelete) Down _ _) gstate = do exitSuccess
input (EventKey key Down _ _) g@GameState{player = p@Player{position = pos, direction = dir}, rotatingLeft = left, rotatingRight = right, bullets = bs, state = st, enemies = e, asteroids = as, randomize = rand}   
  | key `elem` [Char 'd', SpecialKey KeyRight] = return g { rotatingRight = True, player = p { rotation = 1 } }
  | key `elem` [Char 'a', SpecialKey KeyLeft] = return g { rotatingLeft = True, player = p { rotation = (-1) } }
  | key `elem` [Char 'w', SpecialKey KeyUp] = return g { player = p { forward = True } }
  | key `elem` [Char ' ', SpecialKey KeySpace] = return g{bullets = (createBullet (tipPosition dir pos 2.5) dir) : bs}
  | key `elem` [Char 'p', SpecialKey KeyEsc] = if st == Play then return g {state = Pause} else return g {state = Play}
    
input (EventKey key Up _ _) g@GameState{player = p, rotatingLeft = left, rotatingRight = right}   
  | key `elem` [Char 'd', SpecialKey KeyRight] = 
      return g { rotatingRight = False, player = p { rotation = setRotation left False left } }
  | key `elem` [Char 'a', SpecialKey KeyLeft] = 
      return g { rotatingLeft = False, player = p { rotation = setRotation False right right } }
  | key `elem` [Char 'w', SpecialKey KeyUp] = 
      return g { player = p { forward = False } }
input _ g = return g  -- Catch-all pattern for other events