-- | This module contains the help functions
--   which help other functions
module Helper where

import Graphics.Gloss
import System.Random
import Data.Fixed (mod')

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)

checkWholeNumber :: Point -> Bool
checkWholeNumber (x,y) = fromIntegral(round x) == x && fromIntegral(round y) == y

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)       

roundPoint :: Point -> Point
roundPoint (x,y) = (fromIntegral(round x), fromIntegral(round y))

getNewRandom :: StdGen -> (Int, StdGen)
getNewRandom gen = randomR (1, 100) gen

newPosition :: Float -> Point -> Point
newPosition dir (x, y) = (x + dx, y - dy) where
                      dx = 1 * cos dirRadians
                      dy = 1 * sin dirRadians
                      normalizedDir = dir `mod'` 360
                      dirRadians = (normalizedDir - 90) * pi / 180

setRotation :: Bool -> Bool -> Bool -> Int
setRotation True False _ = -1   -- Rotate left if only `a` is pressed
setRotation False True _ = 1    -- Rotate right if only `d` is pressed
setRotation False False _ = 0   -- Stop rotating if neither `a` nor `d` is pressed
setRotation _ _ _ = 0           -- Default to stop rotation

tipPosition :: Float -> Point -> Point
tipPosition dir (x, y) = (x + dx, y - dy) where
          dx = tipDistance * cos dirRadians
          dy = tipDistance * sin dirRadians
          tipDistance = 3.5
          normalizedDir = dir `mod'` 360
          dirRadians = (normalizedDir - 90) * pi / 180


