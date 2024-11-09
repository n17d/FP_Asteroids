module Asteroid where

import Graphics.Gloss
import System.Random

data Asteroid = Asteroid
  { asteroidPos :: Point, 
    asteroidSpeed :: Float, 
    aDirection :: Float, 
    size :: Float 

createAsteroid :: IO Asteroid
createAsteroid = do
  x <- randomRIO (-400, 400)
  y <- randomRIO (-300, 300)
  speed <- randomRIO (1, 3)
  dir <- randomRIO (0, 360)
  size <- randomRIO (10, 30)
  return Asteroid {asteroidPos = (x, y), asteroidSpeed = speed, aDirection = dir, size = size}

moveAsteroid :: Asteroid -> Asteroid
moveAsteroid a@Asteroid{asteroidPos = (x, y), aDirection = dir, asteroidSpeed = speed} =
  a {asteroidPos = newPosition dir (x, y)}

updateAsteroids :: [Asteroid] -> [Asteroid]
updateAsteroids = map moveAsteroid

drawAsteroid :: Asteroid -> Picture
drawAsteroid a@Asteroid{asteroidPos = (x, y), size = s} =
  translate x y $ color white $ circleSolid s