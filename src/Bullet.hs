module Bullet where

import Data.List
import Data.Ord
import Graphics.Gloss
import Helper
import System.IO
import System.Random


data Bullet = Bullet
  { 
    bulletPos :: Point, -- Bullet's current position (x, y)
    bulletSpeed :: Float, -- Bullet's speed
    bDirection :: Float, -- Bullet's direction
    lifetime :: Float -- Bullet's lifetime
  }

createBullet :: Point -> Float -> Bullet
createBullet pos dir = Bullet{bulletPos = pos, bulletSpeed = 5, bDirection = dir, lifetime = 0}

moveBullet :: Bullet -> Bullet
moveBullet b@Bullet{bulletPos = (x,y), bDirection = bdir} = b{bulletPos = newPosition bdir (x,y) 1.8}

updateBullet :: Bullet -> [Bullet]
updateBullet b@Bullet{lifetime = life} | life <= 150 = [moveBullet b{lifetime = life + 1}]
                                       | otherwise = []

drawBullet :: Bullet -> [Picture]
drawBullet b@Bullet{bulletPos = (x, y)} = [color white $ translate x y $ circleSolid 0.2]



