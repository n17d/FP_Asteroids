module Main where

import Controller
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Random
import View

main :: IO ()
main = do
  gen <- getStdGen
  playIO
    (FullScreen) -- Or FullScreen
    black -- Background color
    30 -- Frames per second
    initialState{randomize = gen} -- Initial state
    view -- View function
    input -- Event function
    step -- Step function
