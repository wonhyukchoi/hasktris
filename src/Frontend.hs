-- | Module that contains all UI and user interactions.
module Frontend where

import Tetris
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Dimensions of UI and actual game.
gameHeight, gameWidth, fullHeight, fullWidth :: Int  
gameHeight = 1000 
gameWidth  = 200 
fullHeight = 1200
fullWidth  = 500