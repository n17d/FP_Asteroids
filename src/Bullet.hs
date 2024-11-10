module Bullet where

import Graphics.Gloss

data Bullet = Bullet
  { bulletPos :: Point,
    bulletSpeed :: Float,
    bDirection :: Float,
    lifetime :: Float
  }

createBullet :: Point -> Float -> Bullet
createBullet pos dir = Bullet{bulletPos = pos, bulletSpeed = 5, bDirection = dir, lifetime = 0}

moveBullet :: Bullet -> Bullet
moveBullet b@Bullet{bulletPos = (x,y), bDirection = bdir} = b{bulletPos = newPosition bdir (x,y)}

updateBullet :: Bullet -> [Bullet]
updateBullet b@Bullet{lifetime = life} | life <= 150 = [moveBullet b{lifetime = life + 1}]
                                       | otherwise = []

drawBullet :: Bullet -> [Picture]
drawBullet Bullet{bulletPos = (x, y)} = [color white $ translate x y $ circleSolid 0.2]

-- Function to calculate the new position of the bullet
newPosition :: Float -> (Float, Float) -> (Float, Float)
newPosition dir (x, y) = (x + speed * cos (dir * pi / 180), y + speed * sin (dir * pi / 180))
  where speed = 5