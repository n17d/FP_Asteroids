module Animation where

import Graphics.Gloss

data DeathAnimation = DeathAnimation
  { startFrame :: Float, -- When the animation started
    currentFrame :: Float, -- Current frame of the animation
    position :: Point, -- Position where the death occurred
    colour :: Color -- Color of the death animation,
  }
