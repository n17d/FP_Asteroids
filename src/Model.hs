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

-- Datatypes
data State = Pause | Play | GameOver
  deriving (Eq)

data EnemyShip = Easy | Medium | Hard
  deriving (Eq)

data GameState = GameState
  { state :: State,
    player :: Player
  }

data Player = Player
  { position :: Point, -- Player's position
    lives :: Int, -- Number of lives the player has
    speed :: Float, -- Player's movement speed
    direction :: Float -- Player's direction
  }

drawPlayer :: Player -> [Picture]
drawPlayer p@Player{position = (x, y), direction = dir} = [translate x y $ rotate dir $ 
    pictures 
      [ color blue (rectangleSolid 100 20),                      -- Body of the plane
        color red (polygon [(-50, 0), (-80, 30), (-80, -30)]),  -- Left wing
        color red (polygon [(50, 0), (80, 30), (80, -30)]),     -- Right wing
        color green (polygon [(0, 10), (10, 40), (-10, 40)]),   -- Tail fin
        color green (polygon [(0, -10), (10, -40), (-10, -40)]) -- Bottom fin
      ]]

initialState :: GameState
initialState = GameState {player = initialPlayer, state = Play}

initialPlayer :: Player
initialPlayer = Player {position = (0, 0), direction = 0, lives = 3, speed = 0}