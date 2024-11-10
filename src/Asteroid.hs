module Asteroid where

import Animation
import Data.List
import Data.Ord
import Graphics.Gloss
import Helper
import System.IO
import System.Random
import Data.Fixed (mod')

data Asteroid = Asteroid
  { asteroidPos :: Point, 
    asteroidSpeed :: Float, 
    aDirection :: Float, 
    size :: Float
  }   

createAsteroid :: StdGen -> Point -> Asteroid
createAsteroid g (px,py) =
  let (genX, gen1) = split g                     -- Split generator for x-coordinate
      (genY, gen2) = split gen1                   -- Split generator for y-coordinate
      (genSpeed, gen3) = split gen2               -- Split generator for speed
      (genDir, gen4) = split gen3                 -- Split generator for direction
      (genSize, _) = split gen4                   -- Split generator for size

      x = fst $ randomR (-400, 400) genX
      y = fst $ randomR (-300, 300) genY
      speed = fst $ randomR (1, 3) genSpeed
      dir = targetPlayerDirection (x, y) (px, py)
      s = fst $ randomR (10, 30) genSize
  in Asteroid {asteroidPos = (x, y) , asteroidSpeed = speed, aDirection = dir, size = s}

moveAsteroid :: Asteroid -> Asteroid
moveAsteroid a@Asteroid{asteroidPos = (x, y), aDirection = dir, asteroidSpeed = speed} =
  a {asteroidPos = newPosition dir (x, y)}

updateAsteroid :: Asteroid -> [Asteroid]
updateAsteroid a = map moveAsteroid [a]

spawnAsteroid :: Float -> StdGen -> Point -> [Asteroid]
spawnAsteroid t g (x,y)| t `mod'` 120 == 0 = [createAsteroid g (x,y)]
                  | otherwise = []

drawAsteroid :: Asteroid -> [Picture]
drawAsteroid a@Asteroid{asteroidPos = (x, y), size = s} =
  [translate x y $ color white $ circleSolid s]