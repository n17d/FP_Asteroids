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
data GameState = GameState
  { state :: State,
    deathAnimations :: [DeathAnimation],
    elapsedTime :: Float,
    field :: Field,
    pSpaceship :: Player,
    eSpaceships :: [Enemy],
    randomize :: StdGen,
    bullets :: [Bullet],
    score :: (Int, Int)
  }

data Enemy = Enemy
  { name :: EnemyShip, -- Type or name of the enemy ship
    spawnlocation :: Point, -- Where the enemy spawns
    edirection :: Direction, -- Direction the enemy is facing
    targetField :: Point, -- Target location on the field
    speed :: Float, -- Speed of the enemy
    health :: Int, -- Enemy's health points
  }

data Player = Player
  { position :: Point, -- Player's position
    direction :: Direction, -- Player look direction
    lives :: Int, -- Number of lives the player has
    speed :: Float, -- Player's movement speed
  }

data Bullet = Bullet
  { bulletPos :: (Float, Float), -- Bullet's current position (x, y)
    bulletSpeed :: Float -- Bullet's speed
  }

data Field = Field
  { file :: String, -- Filename or identifier of the field
    size :: (Float, Float), -- Dimensions of the field
    asteroids :: [(Point, Point)], -- List of asteroids as tuples of points
    enemySpawn :: Point, -- Spawn point for enemies
    playerSpawn :: Point -- Spawn point for the player
  }

data Direction = North | East | South | West | None deriving (Eq, Show)

instance Show Direction where
show North = "North"
show East = "East"
show South = "South"
show West = "West"

-- | These functions handle updates to the gamestate
updateGameState :: GameState -> GameState

-- These functions check the state of the game
checkAsteroid :: GameState -> Bool

-- | These functions handle movement
movePlayer :: GameState -> Player
moveEnemy :: GameState -> Enemy -> Enemy

-- | These functions handle drawing
drawPlayer :: Player -> [Picture]