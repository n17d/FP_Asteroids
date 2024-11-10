-- | This module contains the help functions
--   which help other functions
module Helper where

import Graphics.Gloss
import System.Random
import Data.Fixed (mod')

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

targetPlayerDirection :: Point -> Point -> Float
targetPlayerDirection (x, y) (px, py) = (90 - angleDegrees + 360) `mod'` 360
  where
    dx = px - x
    dy = py - y
    angleRadians = atan2 dy dx
    angleDegrees = angleRadians * (180 / pi)


