-- | This module contains the help functions
--   which help other functions
module Helper where

import Graphics.Gloss
import System.Random

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