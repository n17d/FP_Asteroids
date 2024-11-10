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

-- Function to handle bullet-enemy collisions
handleCollisions :: [Bullet] -> [Enemy] -> ([Bullet], [Enemy])
handleCollisions bs es = (remainingBullets, remainingEnemies)
  where
    remainingBullets = [b | b <- bs, not (any (isHit b) es)]
    remainingEnemies = concatMap (\e -> if any (`isHit` e) bs then destroyEnemy e else [e]) es

-- | Handle one iteration
step :: Float -> GameState -> IO GameState
step secs g@GameState{player = p, bullets = bs, time = steps, state = st, enemies = es}
  | st == Play = do
      let (newBullets, newEnemies) = handleCollisions (concatMap updateBullet bs) (concatMap updateEnemy es)
      return g{player = updatePlayer p, bullets = newBullets, time = steps + 1, enemies = newEnemies}
  | st == Pause = return g

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyDelete) Down _ _) gstate = do exitSuccess
input (EventKey key Down _ _) g@GameState{player = p@Player{position = pos, direction = dir}, rotatingLeft = left, rotatingRight = right, bullets = bs, state = st, enemies = es}
  | key `elem` [Char 'd', SpecialKey KeyRight] = return g { rotatingRight = True, player = p { rotation = 1 } }
  | key `elem` [Char 'a', SpecialKey KeyLeft] = return g { rotatingLeft = True, player = p { rotation = (-1) } }
  | key `elem` [Char 'w', SpecialKey KeyUp] = return g { player = p { forward = True } }
  | key `elem` [Char ' ', SpecialKey KeySpace] = return g{bullets = (createBullet (tipPosition dir pos) dir) : bs}
  | key `elem` [Char 'p', SpecialKey KeyEsc] = if st == Play then return g {state = Pause} else return g {state = Play}
  | key `elem` [Char 'e'] = return g{enemies = (spawnEnemy (15, 0)) : es}

input (EventKey key Up _ _) g@GameState{player = p, rotatingLeft = left, rotatingRight = right}
  | key `elem` [Char 'd', SpecialKey KeyRight] =
      return g { rotatingRight = False, player = p { rotation = setRotation left False left } }
  | key `elem` [Char 'a', SpecialKey KeyLeft] =
      return g { rotatingLeft = False, player = p { rotation = setRotation False right right } }
  | key `elem` [Char 'w', SpecialKey KeyUp] =
      return g { player = p { forward = False } }
input _ g = return g  -- Catch-all pattern for other events