-- | This module contains the data types GameState, Enemy and Player.
--   which represent the state of the game.
module Model where

import Animation
import Data.List
import Data.Ord
import Graphics.Gloss
import Helper
import System.IO
import System.Random
import Bullet
import Enemy
import Asteroid

-- Datatypes
data State = Pause | Play | GameOver
  deriving (Eq)

data GameState = GameState
  { state :: State,
    player :: Player,
    bullets :: [Bullet],
    randomize :: StdGen,
    asteroids :: [Asteroid],
    rotatingLeft :: Bool,   -- Track if 'a' or left arrow is held down
    rotatingRight :: Bool,  -- Track if 'd' or right arrow is held down
    time :: Float, -- Tracks time
    enemies :: [Enemy]
  }

data Player = Player
  { position :: Point, -- Player's position
    lives :: Int, -- Number of lives the player has
    speed :: Float, -- Player's movement speed
    direction :: Float, -- Player's direction
    rotation :: Int, -- Player's current rotation -1 is left 0 is neutral +1 is right
    forward :: Bool, -- True = forward, False = nothing
    invulnerability :: Bool,
    invulnerabilityTime :: Float
  }

updatePlayer :: Player -> Player
updatePlayer p@Player{rotation = rot, direction = dir, invulnerability = inv, invulnerabilityTime = invtime} | inv && (invtime /= 0) = p{direction = updateDir p, position = updatePos p, invulnerabilityTime = invtime-1}
                                                                                                             | inv && invtime == 0 = p{direction = updateDir p, position = updatePos p, invulnerability = False}
                                                                                                             | otherwise = p{direction = updateDir p, position = updatePos p}

updateDir :: Player -> Float
updateDir p@Player{rotation = rot, direction = dir} | rot == 1 = dir+6
                                                    | rot == (-1) = dir-6
                                                    | otherwise = dir

updatePos :: Player -> Point
updatePos p@Player{position = (x,y), forward = forw, direction = dir} | forw = newPosition dir (x, y) 1
                                                                      | otherwise = (x,y)

drawPlayer :: Player -> [Picture]
drawPlayer p@Player{position = (x, y), direction = dir} =   [ translate x y $ rotate dir $ pictures 
      [ color green (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]

initialState :: GameState
initialState = GameState {player = initialPlayer, state = Play, rotatingLeft = False, rotatingRight = False, bullets = [], time = 0, enemies = [], asteroids = []}

initialPlayer :: Player
initialPlayer = Player {position = (0, 0), direction = 0, lives = 3, speed = 0, rotation = 0, forward = False, invulnerability = False, invulnerabilityTime = 0}

isCollisionBulletAsteroid :: Bullet -> Asteroid -> Bool
isCollisionBulletAsteroid Bullet{bulletPos = (bx, by)} Asteroid{aposition = (ax, ay), size = radius} =
  let distanceSquared = (bx - ax) ^ 2 + (by - ay) ^ 2
      radiusSquared = radius ^ 2
  in distanceSquared <= radiusSquared

isCollisionAsteroidBullet :: Asteroid -> Bullet -> Bool
isCollisionAsteroidBullet Asteroid{aposition = (ax, ay), size = radius} Bullet{bulletPos = (bx, by)} =
  let distanceSquared = (bx - ax) ^ 2 + (by - ay) ^ 2
      radiusSquared = radius ^ 2
  in distanceSquared <= radiusSquared

asteroidIsHitByBullet :: [Bullet] -> Asteroid -> Bool
asteroidIsHitByBullet bullets asteroid = any (`isCollisionBulletAsteroid` asteroid) bullets

bulletIsHitByAsteroid :: [Asteroid] -> Bullet -> Bool
bulletIsHitByAsteroid asteroids bullet  = any (`isCollisionAsteroidBullet` bullet) asteroids

bulletAsteroidCollision :: [Bullet] -> [Asteroid] -> [Asteroid]
bulletAsteroidCollision bs as = remainingAsteroids where remainingAsteroids = filter (not . asteroidIsHitByBullet bs) as

asteroidBulletCollision :: [Bullet] -> [Asteroid] -> [Bullet]
asteroidBulletCollision bs as = remainingBullets where remainingBullets = filter (not . bulletIsHitByAsteroid as) bs

isCollisionAsteroidPlayer :: Asteroid -> Player -> Bool
isCollisionAsteroidPlayer Asteroid{aposition = (ax, ay), size = radius} Player{position = (px, py)} =
  let distanceSquared = (px - ax) ^ 2 + (py - ay) ^ 2
      radiusSquared = radius ^ 2
  in distanceSquared <= radiusSquared

playerIsHitByAsteroid :: [Asteroid] -> Player -> Bool
playerIsHitByAsteroid asteroids player = any (`isCollisionAsteroidPlayer` player) asteroids

asteroidPlayerCollision :: Player -> [Asteroid] -> Player
asteroidPlayerCollision p@Player{lives = hp, invulnerability = inv} as | playerIsHitByAsteroid as p && (not inv) = p{lives = hp-1, position = (0,0), direction = 0, invulnerability = True, invulnerabilityTime = 90}
                                                                       | otherwise = p