-- | Module that holds everything related to the game logic.
module Tetris where

import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Maybe(fromMaybe, isJust)
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss(Color)

-- | Block is a m x n matrix (implemented as Sequence)
-- that says if the block is full or not.
type Field = Seq.Seq (Seq.Seq Occupied)

-- | Whether block is occupied or not.
type Occupied = Maybe Color

-- | Block consists of shape, velocity, and location, and type.
-- Shape is a 4x1 list that holds the location offsets of each block
-- w.r.t to the location of the entire block.
-- The "location" of the entire block is its upper left corner.
data Block = Block {shape     :: [Location]
                   ,location  :: Location
                   ,color     :: Color}
                   deriving (Show)

-- | x,y coordinates. 
type Location  = (Int, Int)
data Tetromino = I | O | T | S | Z | J | L deriving (Show)

type GameState  = State Field Playing
type Playing    = Bool

-- | Size of playing field.
numVertical, numHorizontal :: Int
numVertical   = 20
numHorizontal = 10

-- | Initialize playing field.
initField = Seq.fromList fieldList
  where fieldList = replicate numHorizontal $ replicate numVertical False

translate :: Int -> Int -> Block -> Block
translate xOffset yOffset block =  block {location = updatedLocation}
  where (x,y)           = location block
        updatedLocation = (x + xOffset, y + yOffset)

moveLeft :: Block -> Block
moveLeft = translate (-1) 0

moveRight :: Block -> Block
moveRight = translate 1 0

moveDown :: Block -> Block
moveDown = translate 0 (-1)

counterClockwise :: Block -> Block
counterClockwise block = block {shape = map rotateCC cubeOffsets}
  where cubeOffsets = shape block
        
        rotateCC :: Location -> Location
        rotateCC (x,y) = (-y, x)

clockwise :: Block -> Block
clockwise block = block {shape = map rotateC cubeOffsets}
  where cubeOffsets = shape block

        rotateC :: Location -> Location
        rotateC (x,y) = (y, -x)

moveBlock :: (Block->Block) -> Field -> Block -> Block
moveBlock move field block =  if legalMove then block' else block
  where 
    block'@(Block shape' (x',y') _) = move block
    cubeLocations                   = map (\(x,y)->(x+x',y+y')) shape'
    legalMove                       = any (`isOccupied` field) cubeLocations

isOccupied :: Location -> Field -> Bool
isOccupied (x,y) field = isJust elem
  where row  = fromMaybe Seq.Empty (field Seq.!? x)
        elem = fromMaybe Nothing (row Seq.!? y)

-- | TODO
-- hitRockBottom :: Block -> Field -> Bool

-- -- | TODO
-- updateGame :: (Block->Block) -> Block -> GameState

-- -- | TODO
-- clearRow :: GameState -> GameState

-- -- | TODO
-- gameOver :: Field -> Playing

-- | Function to create new blocks.
mkBlock :: Tetromino -> Block
mkBlock J = Block [] (0,0) Gloss.red
mkBlock _ = error "Not implemented"

testBlock = mkBlock O