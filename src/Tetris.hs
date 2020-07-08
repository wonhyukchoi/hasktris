-- | Module that holds everything related to the game logic.
module Tetris where

-- | The tetris game datatype.
data Tetris = Tetris
  {score :: Int       -- ^ Accumulated score.
  ,level :: Int       -- ^ Increasing levels increase speed.
  ,blocks :: [[Bool]] -- ^ TODO
  }

-- | Dimensions of UI and actual game.
gameHeight, gameWidth, fullHeight, fullWidth :: Int
gameHeight = 1000 
gameWidth  = 200 
fullHeight = 1200
fullWidth  = 500