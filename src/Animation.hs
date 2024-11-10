module Animation where

import Graphics.Gloss

data DeathAnimation = DeathAnimation
  { startFrame :: Float, -- When the animation started
    currentFrame :: Float, -- Current frame of the animation
    dPosition :: Point, -- Position where the death occurred
    colour :: Color, -- Color of the death animation
    redScreen :: Bool -- Flag to indicate if the screen should turn red
  }

createDeathAnimation :: Float -> Point -> Color -> DeathAnimation
createDeathAnimation start pos col = DeathAnimation
  { startFrame = start,
    currentFrame = start,
    dPosition = pos,
    colour = col,
    redScreen = True
  }

drawDeath :: DeathAnimation -> Picture
drawDeath _ = color red $ rectangleSolid 800 600

updateDeathAnimation :: Float -> DeathAnimation -> DeathAnimation
updateDeathAnimation t anim@DeathAnimation{startFrame = start, currentFrame = current, redScreen = red} =
  anim { currentFrame = t, redScreen = if t - start > 20 then False else red }
  
render :: [DeathAnimation] -> Picture
render deathAnimations = pictures $ map drawDeath deathAnimations