module Enemy where

import Data.List
import Data.Ord
import Graphics.Gloss
import Helper
import System.IO
import System.Random
import Bullet

data Difficulty = Easy | Medium | Hard deriving (Eq)

data Enemy = Enemy
  { eposition :: Point,
    edirection :: Float,
    targetPos :: Point,
    espeed :: Float,
    difficulty :: Difficulty
  }

-- Function to check if a bullet hits an enemy
isHit :: Bullet -> Enemy -> Bool
isHit Bullet{bulletPos = (bx, by)} Enemy{eposition = (ex, ey)} =
  sqrt ((bx - ex)^2 + (by - ey)^2) < 5 -- Assuming a hit radius of 5 units

-- Function to update enemy state
updateEnemy :: Enemy -> [Enemy]
updateEnemy e = [e] -- Placeholder for actual update logic

-- Function to handle enemy destruction
destroyEnemy :: Enemy -> [Enemy]
destroyEnemy _ = [] -- Enemy is destroyed, so return an empty list

-- Function to spawn an enemy
spawnEnemy :: Point -> Enemy
spawnEnemy pos = Enemy{eposition = (0, 15), edirection = 180, targetPos = pos, espeed = 1, difficulty = Easy}

-- Function to draw an enemy
drawEnemy :: Enemy -> [Picture]
drawEnemy Enemy{eposition = (x, y), edirection = dir} =
  [translate x y $ rotate dir $ color red $ polygon [(-2, -2), (2, -2), (0, 2)]]