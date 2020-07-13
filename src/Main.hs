-- | Main module.
module Main where

import qualified Tetris
import qualified Frontend

-- TODO: 
-- key presses registered twice
-- gameOver incorrect

-- | Main function.
main :: IO ()
main = Frontend.playGame