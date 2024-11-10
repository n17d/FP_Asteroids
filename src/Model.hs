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

-- Datatypes
data State = Pause | Play | GameOver
  deriving (Eq)

data GameState = GameState
  { state :: State,
    player :: Player,
    bullets :: [Bullet],
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
    forward :: Bool -- True = forward, False = nothing
  }

updatePlayer :: Player -> Player
updatePlayer p@Player{rotation = rot, direction = dir} = p{direction = updateDir p, position = updatePos p}

updateDir :: Player -> Float
updateDir p@Player{rotation = rot, direction = dir} | rot == 1 = dir+6
                                                    | rot == (-1) = dir-6
                                                    | otherwise = dir

updatePos :: Player -> Point
updatePos p@Player{position = (x,y), forward = forw, direction = dir} | forw = Helper.newPosition dir (x, y)
                                                         | otherwise = (x,y)

drawPlayer :: Player -> [Picture]
drawPlayer p@Player{position = (x, y), direction = dir} =   [ translate x y $ rotate dir $ pictures
      [ color green (polygon [(-2, -2), (2, -2), (0, 2)]) ] ]

initialState :: GameState
initialState = GameState {player = initialPlayer, state = Play, rotatingLeft = False, rotatingRight = False, bullets = [], time = 0, enemies = []}

initialPlayer :: Player
initialPlayer = Player {position = (0, 0), direction = 0, lives = 3, speed = 0, rotation = 0, forward = False}