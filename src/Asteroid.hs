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
  { aposition :: Point, 
    aspeed :: Float, 
    adirection :: Float, 
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
      speed = fst $ randomR (0.5, 1.5) genSpeed
      dir = targetPlayerDirection (x, y) (px, py)
      s = fst $ randomR (5, 15) genSize
  in Asteroid {aposition = (x, y) , aspeed = speed, adirection = dir, size = s}

moveAsteroid :: Asteroid -> Asteroid
moveAsteroid a@Asteroid{aposition = (x, y), adirection = adir, aspeed = speed} =
  a {aposition = newPosition adir (x, y) speed}

updateAsteroid :: Asteroid -> [Asteroid]
updateAsteroid a = map moveAsteroid [a]

spawnAsteroid :: Float -> StdGen -> Point -> [Asteroid]
spawnAsteroid t g (x,y)| t `mod'` 120 == 0 = [createAsteroid g (x,y)]
                       | otherwise = []

drawAsteroid :: Asteroid -> [Picture]
drawAsteroid a@Asteroid{aposition = (x, y), size = s} =
  [translate x y $ color white $ circleSolid s]