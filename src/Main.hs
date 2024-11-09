module Main where

import Controller
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Random
import View

main :: IO ()
main = do
  playIO
    (FullScreen) -- Or FullScreen
    black -- Background color
    30 -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function
