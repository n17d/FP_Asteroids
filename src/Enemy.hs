module Enemy where

import Data.List
import Data.Ord
import Graphics.Gloss
import Helper
import System.IO
import System.Random
import Bullet
import Data.Fixed (mod')

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

randomDifficulty :: StdGen -> (Difficulty, StdGen)
randomDifficulty g =
  let (value, newGen) = randomR (0 :: Int, 2) g
      diff = case value of
                0 -> Easy
                1 -> Medium
                2 -> Hard
  in (diff, newGen)

createEnemy :: StdGen -> Enemy
createEnemy g =
  let (genX, gen1) = split g
      (genY, gen2) = split gen1                  
      (genDiff, _) = split gen2
      x = fst $ randomR (-400, 400) genX
      y = fst $ randomR (-300, 300) genY
      (diff, _) = randomDifficulty genDiff
  in Enemy { eposition = (x, y), difficulty = diff, edirection = 0, espeed = 1, targetPos = (0,0)}

spawnEnemy :: Float -> StdGen -> [Enemy]
spawnEnemy t g | t `mod'` 210 == 0 = [createEnemy g]
               | otherwise = []


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
drawEnemy e@Enemy{eposition = (x,y), edirection = dir, difficulty = diff} | diff == Easy = [ translate x y $ rotate dir $ pictures [ color yellow (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]
                                                                          | diff == Medium = [ translate x y $ rotate dir $ pictures [ color orange (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]
                                                                          | otherwise = [ translate x y $ rotate dir $ pictures [ color red (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]