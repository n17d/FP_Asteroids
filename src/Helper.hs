-- | This module contains the help functions
--   which help other functions
module Helper where

import Graphics.Gloss
import System.Random
import Data.Fixed (mod')

newPosition :: Float -> Point -> Float -> Point
newPosition dir (x, y) sp = (x + dx, y - dy) where
                      dx = sp * cos dirRadians
                      dy = sp * sin dirRadians
                      normalizedDir = dir `mod'` 360
                      dirRadians = (normalizedDir - 90) * pi / 180

setRotation :: Bool -> Bool -> Bool -> Int
setRotation True False _ = -1   -- Rotate left if only `a` is pressed
setRotation False True _ = 1    -- Rotate right if only `d` is pressed
setRotation False False _ = 0   -- Stop rotating if neither `a` nor `d` is pressed
setRotation _ _ _ = 0           -- Default to stop rotation

tipPosition :: Float -> Point -> Float -> Point
tipPosition dir (x, y) td = (x + dx, y - dy) where
          dx = td * cos dirRadians
          dy = td * sin dirRadians
          normalizedDir = dir `mod'` 360
          dirRadians = (normalizedDir - 90) * pi / 180

targetPlayerDirection :: Point -> Point -> Float
targetPlayerDirection (x, y) (px, py) = (90 - angleDegrees + 360) `mod'` 360
  where
    dx = px - x
    dy = py - y
    angleRadians = atan2 dy dx
    angleDegrees = angleRadians * (180 / pi)

shortestAngleDifference :: Float -> Float -> Float
shortestAngleDifference targetAngle currentAngle =
  let diff = (targetAngle - currentAngle + 360) `mod'` 360
  in if diff > 180 then diff - 360 else diff


