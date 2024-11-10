module Animation where

import Graphics.Gloss

data DeathAnimation = DeathAnimation
  { startFrame :: Float,
    currentFrame :: Float,
    dPosition :: Point,
    colour :: Color
  }

-- Function to create a death animation
createDeathAnimation :: Float -> Point -> Color -> [DeathAnimation]
createDeathAnimation start pos col = [DeathAnimation start start pos col]

-- Function to draw a death animation
drawDeath :: DeathAnimation -> [Picture]
drawDeath DeathAnimation{dPosition = (x, y), colour = col} =
  [translate x y $ color col $ circleSolid 5] -- Example animation

-- Function to update a death animation
updateDeathAnimation :: DeathAnimation -> [DeathAnimation]
updateDeathAnimation da@DeathAnimation{currentFrame = frame} =
  if frame - startFrame da < 30 -- Animation lasts for 30 frames
  then [da{currentFrame = frame + 1}]
  else []