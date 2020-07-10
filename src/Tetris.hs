-- | Module that holds everything related to the game logic.
module Tetris where

import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Maybe(fromMaybe, isJust)
import Graphics.Gloss(Color, red, green, blue, 
                      yellow, cyan, magenta, orange)

-- | Block is a m x n matrix (implemented as Sequence)
-- that says if the block is full or not.
type Field = Seq.Seq (Seq.Seq Occupied)

-- | Whether block is occupied or not.
type Occupied = Maybe Color

-- | Block consists of shape, velocity, and location, and type.
-- Shape is a 4x1 list that holds the location offsets of each block
-- w.r.t to the location of the entire block.
-- The "location" of the entire block is its upper left corner.
data Block = Block {shape     :: [Offsets]
                   ,location  :: Location
                   ,color     :: Color}
                   deriving (Show)

-- | x,y coordinates. 
type Location  = (Int, Int)
type Offsets   = (Int, Int)
data Tetromino = I | O | T | J | L | S | Z deriving (Show)

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
        
        rotateCC :: Offsets -> Offsets
        rotateCC (x,y) = (-y, x)

clockwise :: Block -> Block
clockwise block = block {shape = map rotateC cubeOffsets}
  where cubeOffsets = shape block

        rotateC :: Offsets -> Offsets
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
hitRockBottom :: Block -> Field -> Bool
hitRockBottom _ = error "Not Implemented"

-- | TODO
updateGame :: (Block->Block) -> Block -> GameState
updateGame _ = error "Not Implemented"

-- | TODO
clearRows :: GameState -> GameState
clearRows _ = error "Not Implemented"

-- | TODO
gameOver :: Field -> Playing
gameOver _ = error "Not Implemented"

initLocation :: Int -> Location
initLocation x = (x, numVertical)

-- | Function to create new blocks.
-- FIXME: allow random spawning of blocks. 
-- FIXME: make the totality of the blocks appear at once.
mkBlock :: Tetromino -> Block
mkBlock I = Block iBlock (initLocation 0) red
  where iBlock = [(0,0), (0,-1), (0,-2), (0,-3)]
mkBlock O = Block oBlock (initLocation 1) blue
  where oBlock = [(0,0), (1,0), (0,-1), (1,-1)]
mkBlock T = Block tBlock (initLocation 2) green
  where tBlock = [(0,0), (1,0), (2,0), (1,-1)]
mkBlock J = Block jBlock (initLocation 3) yellow
  where jBlock = [(0,0), (0,-1), (0,-2), (-1,-2)]
mkBlock L = Block lBlock (initLocation 4) cyan
  where lBlock = [(0,0), (0,-1), (0,-2), (1,-2)]
mkBlock S = Block sBlock (initLocation 5) magenta
  where sBlock = [(0,0), (1,0), (0,-1), (-1,-1)]
mkBlock Z = Block zBlock (initLocation 6) orange
  where zBlock = [(0,0), (0,1), (1,-1), (2,-1)]

testBlock = mkBlock O