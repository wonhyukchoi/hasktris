-- | Main module.
module Main where

import qualified Tetris
import qualified Frontend

-- TODO: 
-- rotation is bugged
-- clearFullRows might not work?
-- gameOver might not be correct
-- occupied counter might start too early?
-- Obvious problem in dumpColor

-- | Main function.
main :: IO ()
main = Frontend.playGame