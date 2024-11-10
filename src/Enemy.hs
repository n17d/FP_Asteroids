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
 { 
    difficulty :: Difficulty, -- Type or name of the enemy ship
    eposition :: Point, -- Where the enemy spawns
    edirection :: Float, -- Direction the enemy is facing
    targetPos :: Point, -- Target location on the field
    espeed :: Float -- Speed of the enemy
}

updateEnemy :: Point -> Float -> Enemy ->  [Enemy]
updateEnemy (px, py) pdir e@Enemy{difficulty = diff}  | diff == Easy = [moveEnemy e{edirection = changeDirection (px, py) e}] 
                                                      | diff == Medium = [moveEnemy e{edirection = changeDirection (tipPosition pdir (px, py) 20) e}] 
                                                      | otherwise = [moveEnemy e{edirection = changeDirection (px, py) e}]

spawnEnemy :: Point -> Enemy
spawnEnemy pos = Enemy{eposition = (0, 15), edirection = 180, targetPos = pos, espeed = 0.5, difficulty = Medium}

moveEnemy :: Enemy -> Enemy
moveEnemy e@Enemy{eposition = (ex, ey), edirection = edir} = e{eposition = newPosition edir (ex, ey) 0.5}

changeDirection :: Point -> Enemy -> Float
changeDirection (x,y) e@Enemy{eposition = (ex, ey), edirection = edir} | shortestDiff > 0 = edir + 4
                                                                       | shortestDiff < 0 = edir - 4
                                                                       | otherwise = edir
                                                                          where
                                                                          targetDir = targetPlayerDirection (ex, ey) (x, y)
                                                                          shortestDiff = shortestAngleDifference targetDir edir

drawEnemy :: Enemy -> [Picture]
drawEnemy e@Enemy{eposition = (x,y), edirection = dir} = [ translate x y $ rotate dir $ pictures [ color red (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]
