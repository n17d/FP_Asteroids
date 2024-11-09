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

updateEnemy :: Enemy -> [Enemy]
updateEnemy e@Enemy{difficulty = diff} | diff == Easy = [e] 
                                       | diff == Medium = [e]
                                       | otherwise = [e] 

spawnEnemy :: Point -> Enemy
spawnEnemy pos = Enemy{eposition = (0, 15), edirection = 180, targetPos = pos, espeed = 1, difficulty = Easy}

drawEnemy :: Enemy -> [Picture]
drawEnemy e@Enemy{eposition = (x,y), edirection = dir} = [ translate x y $ rotate dir $ pictures [ color red (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]
